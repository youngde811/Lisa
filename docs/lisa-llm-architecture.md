# Lisa + LLM Expert System Architecture

## Status (2026-06-25)

- **Phase 1 (Lisa Server/Bridge)**: COMPLETE — Hunchentoot HTTP server on port 8090, JSON via jzon, endpoints for assert-fact, run-inference, conclusions, rule-trace, partial-matches, reset.
- **Phase 2 (Claude Tool Integration)**: COMPLETE — Python driver (`src/llm/claude/driver.py`) with tool-call dispatch loop, supports Anthropic API and Bedrock. System prompt with full MYCIN ontology.
- **Phase 3 (Conversational Flow)**: COMPLETE — `/partial-matches` endpoint enables goal-directed dialogue; Claude uses it to identify missing facts and ask discriminating questions.
- **Phase 4 (Expanded Rulebase)**: Not started — see `docs/next-steps-llm-integration.md`.

The design below was written pre-implementation. Decisions that were speculative at the time are now resolved: bridge = Hunchentoot, client = Python CLI, communication = HTTP + Claude tool-use.

## Project Overview

Combine Lisa (a forward-chaining expert system shell in Common Lisp) with an LLM (Claude) to create a natural-language-accessible medical expert system. The LLM handles human interaction; Lisa handles deterministic inference with explainable rule traces.

- **Lisa repo**: https://github.com/youngde811/Lisa
- **Author**: David Young (youngde811)
- **Domain**: Medical diagnosis (starting with Mycin rulebase subset)
- **Existing starting point**: `examples/mycin.lisp` in Lisa — forward-chaining Mycin rules with certainty factors

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    User (clinician)                  │
│          "Patient has fever, stiff neck,            │
│           blood culture shows gram+ cocci"          │
└────────────────────────┬────────────────────────────┘
                         │ natural language
                         ▼
┌─────────────────────────────────────────────────────┐
│                   LLM (Claude)                       │
│                                                     │
│  1. Parse NL → structured clinical facts            │
│  2. Map to Lisa's ontology (patient, culture,       │
│     gram, morphology, etc.)                         │
│  3. After inference: translate results + explain    │
└──────────┬──────────────────────────▲───────────────┘
           │ structured facts         │ inference results
           │ (assert commands)        │ (fired rules, beliefs)
           ▼                          │
┌─────────────────────────────────────────────────────┐
│                   Lisa Engine                        │
│                                                     │
│  Working Memory: patient facts, cultures, labs      │
│  Rules: Mycin rulebase (forward-chaining)           │
│  Output: organism-identity + belief factors         │
│          + rule trace (which rules fired)           │
└─────────────────────────────────────────────────────┘
```

## Design Decisions

### 1. Translation Boundary: LLM → Lisa

The LLM produces structured output that Lisa can consume. Recommended approach for POC:

- **Intermediate structured format** — Claude emits a structured representation (JSON or similar), and a thin Lisp adapter translates to Lisa assertions.
- Example: `{"fact": "gram", "entity": "organism-1", "value": "positive", "confidence": 0.8}`
- A Lisp-side adapter function reads this and calls `(assert (gram (entity org-1) (value positive) (confidence 0.8)))`
- This gives us a validation layer — we can reject malformed facts before they enter working memory.

Alternative (consider later): have Claude emit raw s-expressions directly. Simpler but trusts LLM syntax more.

### 2. Translation Boundary: Lisa → LLM

After `(run)`, Lisa exports:

- **Conclusions**: organism identities + belief factors
- **Rule trace**: which rules fired, which facts matched each rule's LHS

The LLM narrates this trace in natural language, e.g.:
> "Based on the gram-positive cocci in the blood culture and the patient's compromised host status, the system suggests Staphylococcus with 70% confidence, primarily via Rule-52."

This is the key differentiator from pure-LLM approaches: real auditability and explainability.

### 3. Conversational Fact-Gathering

The LLM manages dialogue — asking follow-up questions when Lisa's rules need facts not yet asserted. This replaces Mycin's original backward-chaining "ask the user" mechanism with natural conversation.

> "You mentioned a blood culture. Was the organism aerobic or anaerobic?"

The system prompt gives Claude awareness of Lisa's full ontology (what facts exist, valid values, required attributes), so it knows what to ask for.

### 4. Communication Mechanism

For POC, Lisa runs as a subprocess or socket server. Claude interacts via tool-use API with tools like:

| Tool | Purpose |
|------|---------|
| `assert-fact` | Push a structured fact into Lisa's working memory |
| `run-inference` | Trigger Lisa's forward-chaining engine |
| `get-conclusions` | Retrieve organism-identity results + belief factors |
| `get-rule-trace` | Retrieve which rules fired and their matching facts |
| `reset-session` | Clear working memory for a new patient |

A lightweight bridge (HTTP server or stdio) sits between Claude's tool calls and a running Lisa image.

## Mycin Rulebase Notes

The existing `examples/mycin.lisp` contains:

- **Classes**: `patient`, `organism`, `culture`, `gram`, `morphology`, `aerobicity`, etc. (using `param-mixin` pattern)
- **Rules**: rule-52, rule-71, rule-73, rule-75, rule-107, rule-165 — each with `:belief` certainty factors (0.4–0.9)
- **Conclusion rule**: lower-salience rule that fires last to output diagnosis with belief
- **Test scenarios**: `culture-1` and `culture-2` functions demonstrating fact assertion and inference

For expanded POC: locate and translate a larger subset of the original Mycin rulebase (approx 450 rules total in the original system). A subset of 20-50 rules covering common bacteremia scenarios would be a strong next step.

## POC Phases

### Phase 1: Lisa Server/Bridge
- Expose Lisa as a service that accepts structured fact input and returns inference results
- Options: HTTP server in the Lisp image, or a REPL wrapper with stdio protocol
- Must return structured output (not just printed text)

### Phase 2: Claude Tool Integration
- Define tools (assert-fact, run-inference, get-conclusions, get-rule-trace, reset-session)
- System prompt with full ontology documentation
- Test against culture-1 and culture-2 scenarios end-to-end

### Phase 3: Conversational Flow
- Claude drives a diagnostic interview
- Iteratively gathers facts, asserts them, checks what's still unknown
- Runs inference when enough facts are present (or periodically to see partial conclusions)

### Phase 4: Expanded Rulebase
- Translate larger Mycin rule subset to Lisa's forward-chaining format
- Expand ontology classes as needed
- Update system prompt to reflect new fact types

## Key Risks / Open Questions

1. **Ontology mapping fidelity** — Can the LLM reliably map free-text clinical descriptions to Lisa's exact fact vocabulary? The system prompt + validation layer are critical here.
2. **Certainty factor propagation** — When the LLM isn't sure about a fact (e.g., "I think the stain was gram-positive"), how do we represent that uncertainty? Pass a reduced confidence to Lisa?
3. **Backward-chaining gap** — Mycin was goal-directed. In forward-chaining, we rely on the LLM to know what facts are still needed. The system prompt must encode this awareness, or Lisa needs a "what rules are partially matched?" query.
4. **Session state** — Does the Lisa image persist across a conversation, or do we re-assert all facts on each turn? Persistent image is simpler but needs lifecycle management.

## Technical Stack

- **Lisa**: Common Lisp (SBCL primary), Rete algorithm, CLOS/MOP
- **LLM**: Claude (Anthropic API, tool-use mode)
- **Bridge**: TBD — likely a small HTTP server in the Lisp image (e.g., Hunchentoot) or stdio JSON protocol
- **Client**: Could be a simple CLI, web UI, or just the API layer for now

## References

- Lisa GitHub: https://github.com/youngde811/Lisa
- Original Mycin: Shortliffe, E.H. (1976). Computer-Based Medical Consultations: MYCIN
- Claude tool-use docs: https://docs.anthropic.com/en/docs/build-with-claude/tool-use