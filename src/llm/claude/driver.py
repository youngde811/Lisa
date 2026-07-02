#!/usr/bin/env python3

"""
;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.

;; MIT License

;; Copyright (c) 2000 David Young

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
"""

"""Lisa-Claude client driver.

Tool-call dispatch loop between Claude (tool-use API) and the Lisa bridge (HTTP).
Reads system prompt and tool definitions from adjacent files.

Optionally captures the interactive session to a markdown transcript. Configuration
precedence is CLI flags > environment variables > built-in defaults. See the
--help output or docs/ for the full flag/env surface.
"""

import argparse
import json
import os
import sys
from datetime import datetime
from pathlib import Path

import anthropic
import httpx

BRIDGE_URL = os.environ.get("LISA_BRIDGE_URL", "http://localhost:8090")

# Per-backend model defaults. Overridable via LISA_MODEL at runtime.
DEFAULT_MODEL_ANTHROPIC = "claude-sonnet-4-6-20250619"
DEFAULT_MODEL_LMS = "claude-opus-4-7"
DEFAULT_MODEL_VERTEX = "claude-opus-4-7"

# Default LMS (Hyperion) endpoint — the value shown by `cvscode --help`.
# Override via CVSCODE_BASE_URL for dev/stage instances.
DEFAULT_LMS_BASE_URL = "https://hyperion-lms-api.prod.cvshealth.com"

# Path where `cvscode auth login` writes LMS session credentials.
LMS_CREDENTIALS_PATH = Path.home() / ".cvscode" / ".lms-credentials.json"

BACKEND_ANTHROPIC = "anthropic"
BACKEND_LMS = "lms"
BACKEND_VERTEX = "vertex"
VALID_BACKENDS = (BACKEND_ANTHROPIC, BACKEND_LMS, BACKEND_VERTEX)

VERBOSITY_LEVELS = ("minimal", "normal", "full")
DEFAULT_FILENAME_PATTERN = "session-{ts}.md"


def make_http_client():
    """Create an httpx client with SSL handling.

    Python 3.13 rejects some corporate proxy CA certs (Basic Constraints not
    marked critical). Set LISA_SSL_VERIFY=0 to disable verification.
    """
    if os.environ.get("LISA_SSL_VERIFY", "1").strip() in ("0", "false", "no"):
        return httpx.Client(verify=False)
    cert_file = os.environ.get("SSL_CERT_FILE")
    if cert_file and Path(cert_file).exists():
        return httpx.Client(verify=cert_file)
    return httpx.Client()


def _lms_credentials_available() -> bool:
    """Return True when either the cvscode credentials file exists or
    CVSCODE_API_KEY is set in the environment."""
    return LMS_CREDENTIALS_PATH.exists() or bool(os.environ.get("CVSCODE_API_KEY"))


def _load_lms_api_key() -> str:
    """Prefer CVSCODE_API_KEY from env; fall back to the cvscode credentials
    file written by `cvscode auth login`."""
    env_key = os.environ.get("CVSCODE_API_KEY")
    if env_key:
        return env_key
    if not LMS_CREDENTIALS_PATH.exists():
        raise RuntimeError(
            f"LMS backend selected but no credentials found. "
            f"Run `cvscode auth login`, set CVSCODE_API_KEY, or choose a different backend."
        )
    with open(LMS_CREDENTIALS_PATH) as fh:
        data = json.load(fh)
    key = data.get("lmsApiKey")
    if not key:
        raise RuntimeError(
            f"{LMS_CREDENTIALS_PATH} is present but missing 'lmsApiKey'. "
            f"Re-run `cvscode auth login`."
        )
    return key


def resolve_backend() -> str:
    """Decide which LLM backend to use.

    Precedence:
      1. LISA_LLM_BACKEND env var ('anthropic', 'lms', or 'vertex').
      2. Auto-detect in order: ANTHROPIC_API_KEY → anthropic;
         cvscode LMS credentials → lms;
         ANTHROPIC_VERTEX_PROJECT_ID → vertex.
      3. Otherwise raise, with a helpful hint.
    """
    explicit = os.environ.get("LISA_LLM_BACKEND", "").strip().lower()
    if explicit:
        if explicit not in VALID_BACKENDS:
            raise RuntimeError(
                f"Invalid LISA_LLM_BACKEND={explicit!r}. "
                f"Expected one of: {', '.join(VALID_BACKENDS)}."
            )
        return explicit
    if os.environ.get("ANTHROPIC_API_KEY"):
        return BACKEND_ANTHROPIC
    if _lms_credentials_available():
        return BACKEND_LMS
    if os.environ.get("ANTHROPIC_VERTEX_PROJECT_ID"):
        return BACKEND_VERTEX
    raise RuntimeError(
        "No LLM backend detected. Set one of:\n"
        "  ANTHROPIC_API_KEY=...                                (direct Anthropic API — default)\n"
        "  `cvscode auth login`  or  CVSCODE_API_KEY=...        (CVS LMS/Hyperion gateway)\n"
        "  ANTHROPIC_VERTEX_PROJECT_ID=..., CLOUD_ML_REGION=... (GCP Vertex AI)\n"
        "Or set LISA_LLM_BACKEND=anthropic|lms|vertex explicitly."
    )


def make_client():
    """Create an LLM client based on the resolved backend.

    - anthropic: reads ANTHROPIC_API_KEY (and optionally ANTHROPIC_BASE_URL
      to point at an internal Anthropic-protocol wrapper). Default when
      ANTHROPIC_API_KEY is present.
    - lms: CVS Hyperion LMS gateway. Reads the API key from
      CVSCODE_API_KEY or ~/.cvscode/.lms-credentials.json (written by
      `cvscode auth login`). Base URL defaults to hyperion-lms-api.prod;
      override with CVSCODE_BASE_URL.
    - vertex: reads ANTHROPIC_VERTEX_PROJECT_ID and CLOUD_ML_REGION and
      uses GCP Application Default Credentials (e.g. from `gcloud auth
      application-default login`).

    Returns (client, model_id).
    """
    http_client = make_http_client()
    backend = resolve_backend()

    model_override = os.environ.get("LISA_MODEL")

    if backend == BACKEND_ANTHROPIC:
        model = model_override or DEFAULT_MODEL_ANTHROPIC
        return anthropic.Anthropic(http_client=http_client), model

    if backend == BACKEND_LMS:
        api_key = _load_lms_api_key()
        base_url = os.environ.get("CVSCODE_BASE_URL", DEFAULT_LMS_BASE_URL)
        model = model_override or DEFAULT_MODEL_LMS
        return (
            anthropic.Anthropic(
                api_key=api_key,
                base_url=base_url,
                http_client=http_client,
            ),
            model,
        )

    if backend == BACKEND_VERTEX:
        project_id = os.environ.get("ANTHROPIC_VERTEX_PROJECT_ID")
        region = os.environ.get("CLOUD_ML_REGION") or os.environ.get(
            "ANTHROPIC_VERTEX_REGION"
        )
        if not project_id or not region:
            raise RuntimeError(
                "Vertex backend requires ANTHROPIC_VERTEX_PROJECT_ID and "
                "CLOUD_ML_REGION to be set."
            )
        model = model_override or DEFAULT_MODEL_VERTEX
        return (
            anthropic.AnthropicVertex(
                project_id=project_id,
                region=region,
                http_client=http_client,
            ),
            model,
        )

    # Unreachable — resolve_backend() already validated.
    raise RuntimeError(f"Unhandled backend: {backend!r}")

HERE = Path(__file__).parent
SYSTEM_PROMPT = (HERE / "system-prompt.md").read_text()
TOOLS = json.loads((HERE / "tools.json").read_text())

TOOL_TO_ENDPOINT = {
    "assert_fact": ("POST", "/assert-fact"),
    "run_inference": ("POST", "/run-inference"),
    "get_conclusions": ("GET", "/conclusions"),
    "get_rule_trace": ("GET", "/rule-trace"),
    "get_partial_matches": ("GET", "/partial-matches"),
    "reset_session": ("POST", "/reset"),
}


def call_bridge(tool_name: str, tool_input: dict) -> dict:
    method, path = TOOL_TO_ENDPOINT[tool_name]
    url = BRIDGE_URL + path
    if method == "POST":
        resp = httpx.post(url, json=tool_input if tool_input else None)
    else:
        resp = httpx.get(url)
    return resp.json()


# ---------------------------------------------------------------------------
# Transcript capture
# ---------------------------------------------------------------------------


def env_bool(name: str, default: bool) -> bool:
    """Interpret an env var as a boolean. Absent → default."""
    val = os.environ.get(name)
    if val is None:
        return default
    return val.strip().lower() in ("1", "true", "yes", "on")


class TranscriptConfig:
    """Resolved transcript configuration for a driver run.

    Precedence: CLI flags > environment variables > built-in defaults.
    """

    def __init__(self, enabled: bool, directory: Path, filename_pattern: str,
                 verbosity: str):
        self.enabled = enabled
        self.directory = directory
        self.filename_pattern = filename_pattern
        self.verbosity = verbosity

    def resolved_path(self) -> Path:
        ts = datetime.now().strftime("%Y%m%d-%H%M%S")
        return self.directory / self.filename_pattern.format(ts=ts)


def build_transcript_config(args: argparse.Namespace) -> TranscriptConfig:
    """Merge CLI, env, and defaults into a TranscriptConfig."""
    # enabled: --no-transcript wins; then --transcript; then env; else on
    if args.no_transcript:
        enabled = False
    elif args.transcript:
        enabled = True
    else:
        enabled = env_bool("LISA_TRANSCRIPT", True)

    directory = Path(
        args.transcript_dir
        or os.environ.get("LISA_TRANSCRIPT_DIR")
        or "./sessions"
    ).expanduser()

    filename_pattern = (
        args.transcript_file
        or os.environ.get("LISA_TRANSCRIPT_FILE")
        or DEFAULT_FILENAME_PATTERN
    )

    verbosity = (
        args.transcript_verbosity
        or os.environ.get("LISA_TRANSCRIPT_VERBOSITY")
        or "normal"
    ).lower()
    if verbosity not in VERBOSITY_LEVELS:
        raise ValueError(
            f"Invalid transcript verbosity {verbosity!r}. "
            f"Expected one of: {', '.join(VERBOSITY_LEVELS)}."
        )

    return TranscriptConfig(enabled, directory, filename_pattern, verbosity)


class NullTranscript:
    """No-op transcript used when capture is disabled. Same interface as
    Transcript so the driver loop can call every hook unconditionally."""

    enabled = False
    path = None

    def header(self, **_kwargs):
        pass

    def user(self, _text):
        pass

    def assistant(self, _text):
        pass

    def tool_call(self, _name, _input_dict):
        pass

    def tool_result(self, _name, _result_dict):
        pass

    def reset_marker(self, _payload=None):
        pass

    def close(self):
        pass

    def status(self):
        return "transcript: OFF"


class Transcript:
    """Markdown session transcript.

    Verbosity levels:
      - minimal: user turns + assistant text only.
      - normal:  above + tool call names + short input summaries +
                 conclusion payloads.
      - full:    everything, complete JSON for every tool call and result.
    """

    enabled = True

    def __init__(self, path: Path, verbosity: str):
        self.path = path
        self.verbosity = verbosity
        path.parent.mkdir(parents=True, exist_ok=True)
        self._fh = open(path, "w", encoding="utf-8")

    def _write(self, text: str):
        self._fh.write(text)
        self._fh.flush()

    def _fenced_json(self, data) -> str:
        return "```json\n" + json.dumps(data, indent=2) + "\n```\n\n"

    # -- structured hooks -------------------------------------------------

    def header(self, model: str, bridge_url: str, belief_system: str = None):
        lines = [
            "# Lisa/Claude session transcript",
            "",
            f"- Started: {datetime.now().isoformat(timespec='seconds')}",
            f"- Model: `{model}`",
            f"- Bridge: `{bridge_url}`",
        ]
        if belief_system:
            lines.append(f"- Belief system: `{belief_system}`")
        lines.append(f"- Verbosity: `{self.verbosity}`")
        lines.append("")
        lines.append("---")
        lines.append("")
        lines.append("")
        self._write("\n".join(lines))

    def user(self, text: str):
        self._write(f"## Clinician\n\n{text}\n\n")

    def assistant(self, text: str):
        self._write(f"## Assistant\n\n{text}\n\n")

    def tool_call(self, name: str, input_dict: dict):
        if self.verbosity == "minimal":
            return
        header = f"### Tool call: `{name}`\n\n"
        if self.verbosity == "full" or self._is_interesting_call(name):
            self._write(header + self._fenced_json(input_dict))
        else:
            # normal: one-line summary
            summary = self._one_line_summary(name, input_dict)
            self._write(header + (summary + "\n\n" if summary else "\n\n"))

    def tool_result(self, name: str, result_dict: dict):
        if self.verbosity == "minimal":
            return
        if self.verbosity == "full" or self._is_interesting_result(name):
            self._write(
                f"### Tool result: `{name}`\n\n"
                + self._fenced_json(result_dict)
            )
        # normal: results other than conclusions are elided

    def reset_marker(self, payload: dict = None):
        note = ""
        if payload and payload.get("belief_system"):
            note = f" (belief system: `{payload['belief_system']}`)"
        self._write(f"---\n\n**Session reset{note}**\n\n---\n\n")

    def close(self):
        try:
            self._write(
                f"\n---\n\n*Ended {datetime.now().isoformat(timespec='seconds')}*\n"
            )
            self._fh.close()
        except Exception:
            pass

    def status(self):
        return f"transcript: {self.path} (verbosity={self.verbosity})"

    # -- verbosity policy -------------------------------------------------

    _ALWAYS_SHOW_CALLS = {"assert_fact", "reset_session"}
    _ALWAYS_SHOW_RESULTS = {"get_conclusions", "get_rule_trace", "get_partial_matches"}

    def _is_interesting_call(self, name: str) -> bool:
        return name in self._ALWAYS_SHOW_CALLS

    def _is_interesting_result(self, name: str) -> bool:
        return name in self._ALWAYS_SHOW_RESULTS

    def _one_line_summary(self, name: str, input_dict: dict) -> str:
        if not input_dict:
            return f"_(no arguments)_"
        keys = ", ".join(f"{k}={v!r}" for k, v in input_dict.items())
        return f"_{keys}_"


def open_transcript(config: TranscriptConfig):
    if not config.enabled:
        return NullTranscript()
    return Transcript(config.resolved_path(), config.verbosity)


# ---------------------------------------------------------------------------
# Interactive loop
# ---------------------------------------------------------------------------


def parse_args(argv=None):
    parser = argparse.ArgumentParser(
        description="Interactive Claude driver for the Lisa bridge.",
    )
    trans = parser.add_argument_group("transcript capture")
    on_off = trans.add_mutually_exclusive_group()
    on_off.add_argument(
        "--transcript", action="store_true",
        help="Enable transcript capture (overrides LISA_TRANSCRIPT env var).",
    )
    on_off.add_argument(
        "--no-transcript", action="store_true",
        help="Disable transcript capture (overrides LISA_TRANSCRIPT env var).",
    )
    trans.add_argument(
        "--transcript-dir", metavar="PATH",
        help="Directory for transcript files. Default: ./sessions/ "
             "(env: LISA_TRANSCRIPT_DIR).",
    )
    trans.add_argument(
        "--transcript-file", metavar="NAME",
        help="Filename pattern; '{ts}' expands to YYYYmmdd-HHMMSS. "
             f"Default: {DEFAULT_FILENAME_PATTERN!r} "
             "(env: LISA_TRANSCRIPT_FILE).",
    )
    trans.add_argument(
        "--transcript-verbosity", choices=VERBOSITY_LEVELS,
        help="Detail level. Default: normal (env: LISA_TRANSCRIPT_VERBOSITY).",
    )
    return parser.parse_args(argv)


HELP_TEXT = """
Commands:
  quit                  — exit the driver
  reset                 — reset the Lisa session (bridge working memory + messages)
  transcript on         — start a new transcript file
  transcript off        — stop capturing (closes current file)
  transcript where      — print current transcript status
"""


def initial_belief_system() -> str:
    """Ask the bridge which belief system is active by hitting /reset with no
    body change. Returns the belief-system name or None on failure."""
    try:
        payload = call_bridge("reset_session", {})
        return payload.get("belief_system")
    except Exception:
        return None


def run():
    args = parse_args()
    try:
        config = build_transcript_config(args)
    except ValueError as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(2)

    client, model = make_client()
    messages = []

    print("Lisa-Claude Diagnostic Assistant")
    print("Type 'quit' to exit, 'reset' to start a new case, 'help' for commands.")
    print("-" * 50)

    belief_system = initial_belief_system()

    transcript = open_transcript(config)
    if transcript.enabled:
        transcript.header(model=model, bridge_url=BRIDGE_URL,
                          belief_system=belief_system)
        print(transcript.status())
    else:
        print("transcript: OFF")
    if belief_system:
        print(f"belief system: {belief_system}")
    print("-" * 50)

    while True:
        try:
            user_input = input("\nClinician: ").strip()
        except (EOFError, KeyboardInterrupt):
            print()
            break

        if not user_input:
            continue

        # -- meta commands ----------------------------------------------
        lowered = user_input.lower()
        if lowered == "quit":
            break
        if lowered == "help":
            print(HELP_TEXT)
            continue
        if lowered == "reset":
            payload = call_bridge("reset_session", {})
            messages = []
            transcript.reset_marker(payload)
            note = ""
            if payload.get("belief_system"):
                note = f" (belief system: {payload['belief_system']})"
            print(f"\n[Session reset — starting new case{note}]")
            continue
        if lowered == "transcript where":
            print(transcript.status())
            continue
        if lowered == "transcript off":
            if transcript.enabled:
                transcript.close()
            transcript = NullTranscript()
            print("transcript: OFF")
            continue
        if lowered == "transcript on":
            if transcript.enabled:
                print(f"transcript already ON — {transcript.status()}")
                continue
            transcript = Transcript(config.resolved_path(), config.verbosity)
            transcript.header(model=model, bridge_url=BRIDGE_URL,
                              belief_system=belief_system)
            print(transcript.status())
            continue

        # -- normal turn ------------------------------------------------
        messages.append({"role": "user", "content": user_input})
        transcript.user(user_input)

        while True:
            response = client.messages.create(
                model=model,
                max_tokens=1024,
                system=SYSTEM_PROMPT,
                tools=TOOLS,
                messages=messages,
            )

            if response.stop_reason == "tool_use":
                tool_results = []
                assistant_content = response.content

                for block in assistant_content:
                    if block.type == "tool_use":
                        result = call_bridge(block.name, block.input)
                        transcript.tool_call(block.name, dict(block.input))
                        transcript.tool_result(block.name, result)
                        tool_results.append(
                            {
                                "type": "tool_result",
                                "tool_use_id": block.id,
                                "content": json.dumps(result),
                            }
                        )
                    elif block.type == "text" and block.text.strip():
                        print(f"\nAssistant: {block.text}")
                        transcript.assistant(block.text)

                messages.append({"role": "assistant", "content": assistant_content})
                messages.append({"role": "user", "content": tool_results})

            else:
                text = "".join(
                    block.text for block in response.content if block.type == "text"
                )
                if text.strip():
                    print(f"\nAssistant: {text}")
                    transcript.assistant(text)
                messages.append({"role": "assistant", "content": response.content})
                break

    transcript.close()


if __name__ == "__main__":
    run()
