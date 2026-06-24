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
"""

import json
import os
import sys
from pathlib import Path

import anthropic
import httpx

BRIDGE_URL = os.environ.get("LISA_BRIDGE_URL", "http://localhost:8090")
MODEL_ANTHROPIC = "claude-sonnet-4-6-20250619"
MODEL_BEDROCK = "us.anthropic.claude-sonnet-4-6-v1:0"
MODEL_OVERRIDE = os.environ.get("LISA_MODEL")


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


def make_client():
    """Create Anthropic or Bedrock client based on environment.

    Set LISA_USE_BEDROCK=1 to use Bedrock (reads AWS credentials from environment).
    Otherwise uses ANTHROPIC_API_KEY directly.
    """
    http_client = make_http_client()
    if os.environ.get("LISA_USE_BEDROCK", "").strip() in ("1", "true", "yes"):
        region = os.environ.get("AWS_REGION", "us-east-1")
        model = MODEL_OVERRIDE or MODEL_BEDROCK
        return anthropic.AnthropicBedrock(aws_region=region, http_client=http_client), model
    model = MODEL_OVERRIDE or MODEL_ANTHROPIC
    return anthropic.Anthropic(http_client=http_client), model

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


def run():
    client, model = make_client()
    messages = []

    print("Lisa-Claude Diagnostic Assistant")
    print("Type 'quit' to exit, 'reset' to start a new case.")
    print("-" * 50)

    while True:
        try:
            user_input = input("\nClinician: ").strip()
        except (EOFError, KeyboardInterrupt):
            print()
            break

        if not user_input:
            continue
        if user_input.lower() == "quit":
            break
        if user_input.lower() == "reset":
            call_bridge("reset_session", {})
            messages = []
            print("\n[Session reset — starting new case]")
            continue

        messages.append({"role": "user", "content": user_input})

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
                        tool_results.append(
                            {
                                "type": "tool_result",
                                "tool_use_id": block.id,
                                "content": json.dumps(result),
                            }
                        )
                    elif block.type == "text" and block.text.strip():
                        print(f"\nAssistant: {block.text}")

                messages.append({"role": "assistant", "content": assistant_content})
                messages.append({"role": "user", "content": tool_results})

            else:
                text = "".join(
                    block.text for block in response.content if block.type == "text"
                )
                if text.strip():
                    print(f"\nAssistant: {text}")
                messages.append({"role": "assistant", "content": response.content})
                break


if __name__ == "__main__":
    run()
