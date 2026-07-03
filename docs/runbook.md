# Lisa/LLM Runbook — Diagnostic Reasoning with Claude and the MYCIN Rulebase

A hands-on tour of what this system can do. Follow it start to finish and you'll
have seen: forward-chaining inference under two belief algebras, natural-language
fact extraction by Claude, goal-directed dialogue driven by partial matches,
belief combination across independent rules, and Dempster-Shafer's ability to
make uncertainty visible in the output — all while getting a full markdown
transcript you can share.

If something in this runbook doesn't behave the way it says it should, that's a
bug or a rule change — file an issue.

---

## Table of Contents

1. [What you'll see](#what-youll-see)
2. [Prerequisites](#prerequisites)
3. [Start the bridge](#start-the-bridge)
4. [Start the driver](#start-the-driver)
5. [Tour: five demonstrations](#tour-five-demonstrations)
   - [1. Multi-rule belief combination](#1-multi-rule-belief-combination)
   - [2. Partial-matches drive the next question](#2-partial-matches-drive-the-next-question)
   - [3. Switching belief systems mid-conversation](#3-switching-belief-systems-mid-conversation)
   - [4. Conflicting evidence — where DS shines](#4-conflicting-evidence--where-ds-shines)
   - [5. The abdominal anaerobe — narrowing ignorance](#5-the-abdominal-anaerobe--narrowing-ignorance)
6. [Reviewing your session](#reviewing-your-session)
7. [Tuning the transcript](#tuning-the-transcript)
8. [Reference: fact vocabulary and rule catalog](#reference-fact-vocabulary-and-rule-catalog)
9. [Common pitfalls](#common-pitfalls)

---

## What you'll see

The system has two halves that talk over HTTP:

- **Lisa** — a forward-chaining, Rete-based expert system in Common Lisp. Its
  working memory holds *facts*; when facts satisfy a rule's premises, the rule
  fires and adds new facts (typically hypotheses) with a belief factor. The
  belief factor is computed by a pluggable **belief system** — either the
  classic MYCIN certainty factors (CF, a single number in [-1, 1]) or a
  simplified Dempster-Shafer (DS, an interval `[bel, pl]` with an explicit
  ignorance width).
- **Claude** — a large language model driving the conversation with the
  clinician. It doesn't guess diagnoses. It translates natural-language
  observations into structured facts, calls Lisa's endpoints as tool-use
  invocations, and narrates the results with full rule-level traceability.

The MYCIN rulebase currently has **18 rules** covering gram-stain morphology,
site-of-culture context, host status (burn / immunocompromised /
hospital-acquired), travel history, and WBC — including **three disconfirming
rules** that argue *against* a hypothesis (a contradictory stain or oxygen
requirement), which is what lets Dempster-Shafer's conflict handling produce
plausibility below 1.0. See
[`docs/clinician-scenarios.md`](clinician-scenarios.md) for the full annotated
scenario catalog.

**Dempster-Shafer is the default belief system.** CF is a one-line env-var
switch away.

---

## Prerequisites

- SBCL with Quicklisp (project loads via `lisa.asd` / `lisa-bridge.asd`)
- Python 3.10+ with the `anthropic`, `httpx`, and (recommended) `rich`
  packages. `rich` gives you properly rendered tables, bold, and headings
  in the terminal; without it the driver still works but Claude's markdown
  prints as raw text. `pip install anthropic httpx rich`
- One of the following LLM-backend configurations (the driver auto-detects
  in this order, or set `LISA_LLM_BACKEND=anthropic|lms|vertex` to force one):
  - **Anthropic direct (default for public users)**: `ANTHROPIC_API_KEY` in
    the environment. Optionally set `ANTHROPIC_BASE_URL` if routing through
    an internal Anthropic-protocol wrapper — the SDK reads it automatically.
  - **CVS LMS / Hyperion** (for CVS engineers): run `cvscode auth login`
    once. The driver reads `~/.cvscode/.lms-credentials.json` automatically
    — no env vars needed. Override the endpoint with `CVSCODE_BASE_URL` or
    the API key with `CVSCODE_API_KEY` if you need to.
  - **GCP Vertex AI**: run `gcloud auth application-default login` once,
    then set `ANTHROPIC_VERTEX_PROJECT_ID` and `CLOUD_ML_REGION`.

That's it — no other services required.

---

## Start the bridge

From the project root, in an SBCL REPL:

```lisp
(load "lisa.asd")
(asdf:load-system :lisa)

(in-package :lisa-user)
(load "examples/mycin.lisp")     ; loads classes, 18 rules, culture-* driver funcs

(asdf:load-system :lisa-bridge)
(lisa-bridge:start)              ; port 8090
```

You should see:

```
Lisa bridge started on port 8090 (belief system: Dempster-Shafer (simplified)).
```

**Prefer certainty factors?** Set the env var before starting SBCL:

```bash
LISA_BELIEF_SYSTEM=cf sbcl
```

Then run the same load sequence. The startup line will read
`(belief system: Certainty Factors (Shortliffe-Buchanan))`.

Sanity check the bridge from another shell:

```bash
curl -s http://localhost:8090/health
# → {"status":"ok"}
```

To stop the bridge later: `(lisa-bridge:stop)` at the REPL.

---

## Start the driver

In another terminal, pick a backend:

```bash
# Direct Anthropic API (default when ANTHROPIC_API_KEY is set)
export ANTHROPIC_API_KEY=...

# — or — CVS LMS / Hyperion (for CVS engineers):
# cvscode auth login   (once; writes ~/.cvscode/.lms-credentials.json)
# no env vars required — the driver reads the creds file automatically

# — or — GCP Vertex AI:
# gcloud auth application-default login   (once)
# export ANTHROPIC_VERTEX_PROJECT_ID=your-gcp-project
# export CLOUD_ML_REGION=us-east5

python src/llm/claude/driver.py
```

The driver auto-detects the backend from what's present, or force it with
`LISA_LLM_BACKEND=anthropic|lms|vertex`. To pin a specific model regardless
of backend defaults, set `LISA_MODEL=claude-...`.

You'll get:

```
Lisa-Claude Diagnostic Assistant
Type 'quit' to exit, 'reset' to start a new case, 'help' for commands.
--------------------------------------------------
transcript: sessions/session-20260702-121500.md (verbosity=normal)
belief system: Dempster-Shafer (simplified)
--------------------------------------------------

Clinician:
```

You are now the clinician. Type case descriptions in plain English. Claude
will extract facts, call Lisa's endpoints, and narrate.

Meta-commands you can type at the `Clinician:` prompt:

| Command | Effect |
|---|---|
| `help` | Show the command list |
| `reset` | Wipe Lisa's working memory and start a new case (transcript continues with a marker) |
| `transcript on` / `off` / `where` | Runtime control of session capture |
| `quit` | Exit the driver |

---

## Tour: five demonstrations

Each demo below has a **paste-ready clinician script** — copy the italicized
lines one at a time into the driver. Some demos have branch points; take them
if you want to explore.

### 1. Multi-rule belief combination

**Goal**: See the same organism concluded by more than one rule, and watch the
belief combine (upward, but with diminishing returns — that's the algebra
working correctly).

**Setup**: default DS, fresh session.

Paste:

> *I have a 27-year-old female burn patient. She's obviously immunocompromised
> from the burn. Blood culture from three days ago shows gram-negative rods,
> aerobic.*

Claude will assert `burn=serious`, `compromised-host=t`, `culture-site=blood`,
`culture-age=3`, `gram=neg`, `morphology=rod`, `aerobicity=aerobic`, then run
inference.

**Expected**: four rules fire — two on the pseudomonas branch (burn rule +
compromised-host rule, whose beliefs combine), plus one enterobacteriaceae
rule and one klebsiella rule that also match the aerobic-gram-neg-rod +
compromised-host pattern. The `/conclusions` payload includes:

```json
{
  "conclusions": [
    {"value": "enterobacteriaceae", "belief": {"bel": 0.80, "pl": 1.0, "ignorance": 0.20}},
    {"value": "pseudomonas",        "belief": {"bel": 0.76, "pl": 1.0, "ignorance": 0.24}},
    {"value": "klebsiella",         "belief": {"bel": 0.50, "pl": 1.0, "ignorance": 0.50}}
  ],
  "belief_system": "Dempster-Shafer (simplified)"
}
```

(Conclusion ordering isn't significant — the payload isn't sorted by belief,
so a given run may list pseudomonas before enterobacteriaceae or vice versa.
Compare against `docs/sample-session.md`, where the same case lists
pseudomonas first.)

**Read this carefully**: pseudomonas ends up at 0.76 even though *neither*
underlying rule has a belief that high (0.4 for burn, 0.6 for compromised).
That's belief combination — two independent lines of evidence for the same
hypothesis reinforcing each other via `combine-beliefs`.

Also note the *ignorance widths* across the three hypotheses (0.20 / 0.24 /
0.50). Klebsiella is a real hit on this evidence — the
`aerobic-gram-neg-rod-in-compromised-host-suggests-klebsiella` rule (belief
0.5) matches — but with only one rule supporting it and a moderate rule
belief, DS honestly reports "50% supported, 50% still unresolved." That's
exactly the sort of nuance CF collapses into a single number.

Ask Claude a follow-up: *"Why is pseudomonas at 0.76 and not 1.0?"* — it will
narrate the two rules and explain that DS is conservative: two moderate-belief
rules combine to a higher-belief-but-still-not-certain result. You can also
ask *"why is klebsiella's ignorance so wide?"* and it should point at the
single-rule support with moderate rule belief.

### 2. Partial-matches drive the next question

**Goal**: See Claude look at *what facts are missing* to decide what to ask
next, rather than freewheeling.

Type `reset`, then paste:

> *Elderly patient, septic, WBC is 2.1 — quite low. Blood culture:
> gram-negative rods.*

Claude will assert `white-blood-count=low`, `culture-site=blood`, `gram=neg`,
`morphology=rod`. That's enough to fire the low-WBC salmonella rule (0.55).
But before it stops, Claude should call `get_partial_matches` — you'll see
several rules with `matched=3, missing=[aerobicity=aerobic|anaerobic]` or
similar.

Claude should then ask: *"Do you have aerobicity results yet?"* rather than
running inference immediately. This is the goal-directed dialogue loop in
action.

Reply *aerobic* and let it complete. You'll get salmonella (0.55, wide
ignorance) alongside enterobacteriaceae (0.8) — a nice split differential.

### 3. Switching belief systems mid-conversation

**Goal**: Compare the same case under both algebras. This is where DS's
information richness becomes obvious.

Still in the same driver session, type `reset` and then say:

> *Please switch to certainty factors and start over.*

Claude will call `reset_session` with `{"belief_system": "cf"}`. The driver
will echo `[Session reset — starting new case (belief system: Certainty
Factors (Shortliffe-Buchanan))]`.

Now paste the burn-patient case from Demo 1 again:

> *27-year-old burn patient, immunocompromised. Blood culture: gram-negative
> rods, aerobic, three days.*

Expected CF conclusions:

```json
{
  "conclusions": [
    {"value": "enterobacteriaceae", "belief": 0.8},
    {"value": "pseudomonas",        "belief": 0.76},
    {"value": "klebsiella",         "belief": 0.5}
  ],
  "belief_system": "Certainty Factors (Shortliffe-Buchanan)"
}
```

Notice what's *missing* compared to Demo 1: no ignorance interval. CF gives
you a point estimate; DS shows you how wide the confidence interval is.

**Bonus**: you can also switch from the shell (useful for scripting):

```bash
curl -sX POST http://localhost:8090/reset \
     -H 'content-type: application/json' \
     -d '{"belief_system":"ds"}'
```

Or set the default for the whole bridge lifetime with `LISA_BELIEF_SYSTEM` at
startup.

### 4. Conflicting evidence — where DS shines

**Goal**: Show how DS makes *evidential conflict* visible in the output as a
plausibility ceiling below 1.0, where CF collapses it to a bare point.

Switch back to DS: *"Reset and use Dempster-Shafer."*

Then:

> *Same burn patient as before. Microbiologist is hedging — probably
> gram-negative rods, but possibly gram-positive. The stain wasn't great.
> Blood culture, anaerobic organism.*

Claude will assert two competing gram facts with confidence values:
`gram=neg` at 0.8, `gram=pos` at 0.6. Under DS these normalize to
`[bel=0.8, pl=1.0]` and `[bel=0.6, pl=1.0]` — both live in working memory with
different belief strengths.

When you run inference, the `anaerobic-gram-neg-rod-in-blood-suggests-bacteroides`
rule fires for bacteroides (a gram-negative anaerobe). But the gram-*positive*
reading now triggers a **disconfirming** rule —
`gram-pos-stain-argues-against-gram-neg-organism` (belief −0.7) — which argues
*against* bacteroides (and against pseudomonas). Dempster's rule of combination
folds the conflicting evidence in and renormalizes, pulling plausibility below
1.0:

```json
{"value": "bacteroides", "belief": {"bel": 0.60, "pl": 0.83, "ignorance": 0.23}}
{"value": "pseudomonas", "belief": {"bel": 0.51, "pl": 0.80, "ignorance": 0.29}}
```

The **plausibility ceiling** (0.83, not 1.0) is the tell: something argued
against this organism. That's the story CF can't tell — run the identical case
under CF and you get bare points (`bacteroides ≈ 0.52`, `pseudomonas ≈ 0.39`)
with no signal that the gram-positive reading created any conflict. Note too
that DS's belief (0.60) sits *above* CF's (0.52): Dempster redistributes the
conflict mass across the frame rather than simply subtracting it, so `bel` and
`pl` each carry a different part of the story.

> **See it for real:** [`docs/sample-session-ds-conflict.md`](sample-session-ds-conflict.md)
> is a captured transcript of exactly this case driven end-to-end through Claude
> and the bridge — the DS conclusions (`bacteroides {bel 0.60, pl 0.83}`), the
> disconfirming rule firing, and the CF side-by-side, all with Claude's
> narration.

### 5. The abdominal anaerobe — narrowing ignorance

**Goal**: See DS combination *narrowing* the ignorance interval as
independent evidence accumulates. This is the mirror image of Demo 4.

`reset`, then:

> *Post-op appendectomy patient. Blood culture came back positive too.
> Anaerobic gram-negative rods.*

Claude should assert: `culture-site=blood`, `infection-site=abdominal` (on
the patient), `gram=neg`, `morphology=rod`, `aerobicity=anaerobic`.

Two bacteroides rules fire — the classic PAIP one (0.9) and the new abdominal
one (0.8). Watch what combination does:

```json
{"value": "bacteroides",
 "belief": {"bel": 0.98, "pl": 1.0, "ignorance": 0.02}}
```

`bel = 0.98`, `ignorance = 0.02`. This is a near-certain conclusion, and the
interval reflects it. Compare with any single-rule conclusion from Demo 2 or
3, where ignorance is 0.2 or wider.

**Teaching moment**: DS combination *reduces* ignorance when evidence
reinforces. Divergent evidence *widens* it (Demo 4). CF just moves a single
number around and you can't tell the two situations apart from the output.

---

## Reviewing your session

Every session goes to `sessions/session-YYYYmmdd-HHMMSS.md` by default. It's
plain markdown — open it in any editor or paste it into a shared doc.

Structure:

```
# Lisa/Claude session transcript

- Started: 2026-07-02T12:15:00
- Model: `claude-sonnet-5`
- Bridge: `http://localhost:8090`
- Belief system: `Dempster-Shafer (simplified)`
- Verbosity: `normal`

---

## Clinician
<your message>

## Assistant
<Claude's narration>

### Tool call: `assert_fact`
```json
{ ... }
```

### Tool result: `get_conclusions`
```json
{ "conclusions": [...], "belief_system": "..." }
```
```

Reset markers in the file make it easy to find where each new case started.

---

## Tuning the transcript

Configuration precedence: **CLI > env vars > defaults**.

| Concern | CLI flag | Env var | Default |
|---|---|---|---|
| Enable/disable | `--transcript` / `--no-transcript` | `LISA_TRANSCRIPT` (`1`/`0`) | on |
| Output dir | `--transcript-dir PATH` | `LISA_TRANSCRIPT_DIR` | `./sessions/` |
| Filename pattern | `--transcript-file NAME` | `LISA_TRANSCRIPT_FILE` | `session-{ts}.md` |
| Verbosity | `--transcript-verbosity {minimal,normal,full}` | `LISA_TRANSCRIPT_VERBOSITY` | `normal` |

Verbosity levels:
- **minimal** — user turns + assistant text only. Best for sharing a
  conversation with a non-technical reader.
- **normal** — the above, plus tool call names, short input summaries, and
  full conclusion / rule-trace / partial-match payloads. Best for review.
- **full** — every tool call and result with complete JSON bodies. Best for
  debugging.

Runtime commands at the `Clinician:` prompt: `transcript on|off|where`. Useful
if a portion of a session is sensitive or exploratory and you don't want it
captured.

## Terminal rendering

Claude produces markdown: tables, bold, headings, lists. By default the
driver renders that markdown to the terminal using the `rich` package —
tables become properly aligned boxes, `**bold**` becomes actual bold text,
`###` becomes visible headings. Highly recommended for interactive use.

- `pip install rich` if you don't have it. Without `rich` the driver falls
  back to raw markdown output (functional but harder to read).
- `--plain` on the CLI or `LISA_PLAIN=1` in the environment disables rich
  rendering and prints raw markdown, useful when piping the driver's
  output to another tool or when the terminal doesn't handle ANSI well.

Transcripts always contain raw markdown regardless of terminal display —
that's what makes them portable to any viewer.

---

## Reference: fact vocabulary and rule catalog

**Organism facts** (attach to `organism-1`, `organism-2`, ...):

| Fact | Values |
|---|---|
| `gram` | pos, neg |
| `morphology` | rod, coccus |
| `aerobicity` | aerobic, anaerobic |
| `growth-conformation` | clumps, chains |

**Patient facts** (attach to `patient-1`):

| Fact | Values |
|---|---|
| `burn` | serious |
| `compromised-host` | t |
| `hospital-acquired` | t |
| `recent-travel` | tropical |
| `white-blood-count` | low |
| `infection-site` | respiratory, abdominal |

**Culture facts** (no entity):

| Fact | Values |
|---|---|
| `culture-site` | blood |
| `culture-age` | integer (days) |

**Rule catalog** is enumerated in `src/llm/claude/system-prompt.md` (and
Claude has it in context — you can always ask *"which rules do you have?"*
and it will read them back).

For the full annotated seven scenarios with expected differentials under
both belief systems, see [`docs/clinician-scenarios.md`](clinician-scenarios.md).

---

## Common pitfalls

- **"Pseudomonas only came out at 0.6, not the higher combined belief I
  expected."** You probably only had one pseudomonas rule fire. To see
  combination, the case needs to hit at least two of: burn=serious,
  compromised-host=t, hospital-acquired=t (plus the standard gram-neg-rod
  facts). See [`docs/clinician-scenarios.md`](clinician-scenarios.md#scenario-1--paip-culture-1-baseline).
- **"Claude ran inference too early."** Ask it to check partial matches
  first, or say something like *"before you run inference, what's still
  missing?"* — it will call `get_partial_matches` and describe what would
  discriminate the differential.
- **"Wide ignorance intervals under DS."** Wide intervals are correct when
  evidence is weak or contradictory. If you want them to narrow, add more
  facts that trigger additional rules concluding the same organism.
- **"The bridge won't start with `LISA_BELIEF_SYSTEM=bayes`."** Only `cf`,
  `certainty-factors`, `ds`, and `dempster-shafer` are valid. Unknown values
  fail loudly on purpose — no guessing.
- **Session files piling up in `sessions/`?** They're gitignored. Delete at
  will, or set `LISA_TRANSCRIPT_DIR` somewhere ephemeral.
- **`ModuleNotFoundError: anthropic`.** Install driver deps in a venv:
  `python -m venv .venv && .venv/bin/pip install anthropic httpx rich`.
- **Tables and bold showing as raw markdown (`| a | b |`, `**bold**`).**
  You don't have `rich` installed. `pip install rich` and rerun the
  driver, or accept the raw output. See the [Terminal rendering](#terminal-rendering)
  section.

---

## What next?

- Read `docs/clinician-scenarios.md` for two more scenarios (respiratory
  strep in a compromised host; tropical traveler with gram-neg rod).
- Read `docs/lisa-llm-architecture.md` for the design rationale.
- Modify `examples/mycin.lisp` to add your own rules — the driver picks them
  up automatically after a bridge reset, no schema changes needed for rules
  that use the existing fact vocabulary.

Happy diagnosing.
