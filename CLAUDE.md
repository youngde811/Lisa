# Lisa — Lisp-based Intelligent Software Agents

Forward-chaining expert system shell in Common Lisp (Rete algorithm, CLOS/MOP, certainty factors). Integrated with Claude via tool-use for natural-language medical diagnosis (MYCIN rulebase).

## Build & Load

Requires SBCL with Quicklisp. From the SBCL REPL at project root:

```lisp
;; Load Lisa core
(load "lisa.asd")
(asdf:load-system :lisa)

;; Load the MYCIN rules (classes + rules, no assertions)
(in-package :lisa-user)
(load "examples/mycin.lisp")

;; Load and start the LLM bridge
(asdf:load-system :lisa-bridge)
(lisa-bridge:start)  ; starts on port 8090
```

To stop: `(lisa-bridge:stop)`

## Project Structure

```
lisa.asd              — Core system definition (depends on log4cl)
lisa-bridge.asd       — Bridge system (depends on lisa, hunchentoot, jzon, bordeaux-threads)
src/
  core/               — Rete engine, rules, facts, conflict resolution
  belief-systems/     — Certainty factor support
  rete/reference/     — Rete network nodes and compiler
  llm/bridge/         — HTTP bridge for LLM integration
    package.lisp      — :lisa-bridge package
    session.lisp      — Entity registry, session reset
    server.lisp       — Hunchentoot start/stop
    handlers.lisp     — REST endpoints
  llm/claude/         — Claude tool-use integration
    driver.py         — Python client: tool-call dispatch loop between Claude and Lisa bridge
    tools.json        — Tool schemas (assert_fact, run_inference, get_conclusions, etc.)
    system-prompt.md  — Clinical diagnostic system prompt with fact ontology
examples/
  mycin.lisp          — MYCIN rules (6 rules, certainty factors, culture-1/culture-2 scenarios)
bin/
  test-culture-1.sh   — End-to-end bridge test (curl-based)
  run-mycin.sh        — Same as test-culture-1.sh (legacy name)
```

## Bridge Endpoints (port 8090)

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/health` | GET | Health check |
| `/assert-fact` | POST | Assert a fact: `{fact_type, value, entity?, entity_class?, confidence?}` |
| `/run-inference` | POST | Fire rules (captures rule trace) |
| `/conclusions` | GET | Get organism-identity results + belief factors |
| `/rule-trace` | GET | Get which rules fired last run |
| `/reset` | POST | Clear working memory and entity registry |

## Testing the Bridge

```bash
# Start the bridge first (see Build & Load above), then:
./bin/test-culture-1.sh
```

Expected: culture-1 scenario produces pseudomonas (0.6) and enterobacteriaceae (0.8).

## Key Packages

- `lisa` / `lisa-user` — Core engine and user-facing DSL (defrule, assert, run, reset)
- `belief` — Certainty factor operations (belief-factor, combine-beliefs)
- `lisa-bridge` — HTTP bridge (start, stop, reset-session)

## LLM Integration Status

Both phases are complete:

- **Phase 1 — HTTP Bridge**: Hunchentoot server exposing Lisa's inference engine as REST endpoints (assert-fact, run-inference, conclusions, rule-trace, partial-matches, reset).
- **Phase 2 — Claude Tool-Use**: Python driver (`src/llm/claude/driver.py`) implementing a tool-call dispatch loop between Claude and the Lisa bridge. Includes tool schemas for all 6 endpoints, a system prompt with the MYCIN clinical ontology and uncertainty-mapping guidelines, and goal-directed dialogue via `/partial-matches`.

### Running the Clinician Driver

```bash
# Requires: anthropic Python package, Lisa bridge running on port 8090
export ANTHROPIC_API_KEY=sk-...
python src/llm/claude/driver.py

# Or via Bedrock:
export LISA_USE_BEDROCK=1
python src/llm/claude/driver.py
```

Architecture plan: `/Users/a254571/share/docs/lisa-llm-architecture.md`

## Style Notes

- Common Lisp conventions: kebab-case, `defvar` for specials with earmuffs
- ASDF for system definition, Quicklisp for dependency management
- Bridge uses jzon for JSON (not cl-json) — `com.inuoe.jzon:parse` / `com.inuoe.jzon:stringify`

## Editing README.md

README.md is large (~290 lines) with extensive markdown code blocks (triple backticks, tables, inline code). This makes it difficult to edit with standard tools:

- The `Write` tool truncates when the content contains many backtick-heavy code blocks
- Shell heredocs (`cat << 'EOF'`) also break on the embedded backticks and special characters
- **What works**: Write the first portion with `Write`, then append the rest via a Python script (`open(path, 'a')`) which handles quoting correctly. Alternatively, use `Edit` for small targeted changes within the file — just keep each edit under ~60 lines to avoid uniqueness issues.