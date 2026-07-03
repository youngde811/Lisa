# Next Steps: Lisa/LLM Integration Research

## Context

> **Note:** This section describes the state *before* the 2026-07-02 work
> below. The rulebase has since grown from 6 → 15 rules and Dempster-Shafer
> has been added as a second (now default) belief system — see the Progress
> section at the end.

Phase 1 (HTTP bridge) and Phase 2 (Claude tool-use clinician driver) are complete. The system demonstrates a hybrid architecture where Claude handles natural-language interaction and Lisa handles deterministic forward-chaining inference with certainty factors and full rule traceability. At the time of writing, the MYCIN rulebase has 6 rules covering a narrow slice of infectious disease identification.

We want to push this further — not toward a full MYCIN port (clinically irrelevant today, and the backward-chaining mismatch makes it impractical at scale), but toward a richer demonstration of what the hybrid architecture can do.

## Environment

- Development on personal Mac, using the Anthropic-protocol client (points at api.anthropic.com by default; set `ANTHROPIC_BASE_URL` to route through an internal wrapper)
- Branch: master (or feature branches as needed)
- Driver: `src/llm/claude/driver.py` with `ANTHROPIC_API_KEY`

## Three Research Threads

### 1. Expand the Rulebase (6 → 25-30 rules)

**Goal**: Enough rules to produce genuinely interesting multi-hypothesis dialogues where `/partial-matches` surfaces competing diagnoses and belief factors combine across evidence paths.

**Approach**:
- Author rules *for* forward-chaining (not translated from backward-chaining MYCIN originals)
- Cover more organism classes: add fungi, viruses, parasites beyond just bacteria
- Add differential paths where the same evidence supports multiple hypotheses at different confidence levels
- Include rules that combine — e.g., two independent pieces of evidence both pointing to the same organism, so the belief algebra gets exercised
- Add patient-context rules (immunocompromised, recent travel, hospital-acquired vs community-acquired)

**Success looks like**: A diagnostic conversation where Claude navigates 3-4 competing hypotheses, asks discriminating questions guided by partial-matches, and the final conclusion shows belief combination from multiple fired rules.

**Files to modify**: `examples/mycin.lisp`, `src/llm/claude/system-prompt.md` (update ontology), `src/llm/claude/tools.json` (if new fact types added)

### 2. Belief Algebra Review

**Goal**: Determine whether Lisa's current certainty factor implementation is the best choice, or whether a different weighting algebra would produce more explainable and intuitive results.

**Current state**: Lisa's `src/belief-systems/` implements Shortliffe/Buchanan certainty factors (CF). The `belief` package provides `belief-factor` and `combine-beliefs`.

**Known issues with classic CF**:
- The combination formula can behave counterintuitively with conflicting evidence
- The -1 to +1 range with asymmetric combination was designed for a specific era
- Order-independent but not always intuitive to clinicians or end users

**Alternatives to explore**:
- **Dempster-Shafer theory** — handles uncertainty and ignorance separately; good for combining independent evidence sources
- **Probabilistic soft logic** — continuous truth values [0,1] with well-defined combination semantics
- **Simplified Bayesian** — prior/posterior updating that Lisa's forward-chaining could drive (assert priors, rules update posteriors)
- **Weighted evidence accumulation** — simpler than full Bayes, more intuitive than CF

**Evaluation criteria**:
- Explainability (can Claude narrate *why* the confidence is what it is?)
- Behavior with conflicting evidence
- Behavior when multiple rules support the same conclusion
- Implementation complexity within Lisa's existing architecture

**Approach**: Start with a deep read of `src/belief-systems/`. Understand what's there. Then prototype an alternative algebra alongside it (not replacing — Lisa should support pluggable belief systems). Compare results on the same scenario with both algebras.

### 3. Architecture Validation

**Goal**: Confirm the bridge/driver/partial-matches loop scales to a richer rulebase without degradation in dialogue quality or explainability.

**Things to watch for**:
- Does `/partial-matches` return too many candidates when the rulebase grows? (May need ranking or filtering)
- Does Claude get confused choosing what to ask next with many competing hypotheses?
- Does the rule trace remain explainable when 5-10 rules fire instead of 1-2?
- Does the system prompt need restructuring to handle a larger ontology?

**Approach**: This thread is exercised implicitly by threads 1 and 2. Run real diagnostic conversations after each expansion and note where the architecture creaks.

## Suggested Sequence

1. **Start with thread 2** — read `src/belief-systems/`, understand the current implementation, form an opinion before adding complexity
2. **Then thread 1** — expand the rulebase with the chosen (or dual) belief algebra in mind
3. **Thread 3** — continuous validation as we go

## Key Files

```
src/belief-systems/          — Current certainty factor implementation
examples/mycin.lisp          — Current 6-rule rulebase
src/llm/bridge/handlers.lisp — Bridge endpoints (partial-matches logic lives here)
src/llm/claude/driver.py     — Clinician driver
src/llm/claude/system-prompt.md — Ontology and dialogue strategy
src/llm/claude/tools.json    — Tool schemas
```

## Architecture Reference

Full architecture plan: `docs/lisa-llm-architecture.md`

---

## Progress (2026-07-02)

### Completed

- **Thread 2 — Belief algebra**: Pluggable belief-system protocol
  (`src/belief-systems/protocol.lisp`) landed with two implementations:
  certainty factors (`certainty-factors/`) and simplified Dempster-Shafer
  (`dempster-shafer/`). Convenience switch: `(belief:use-system
  :certainty-factors | :dempster-shafer)`.
- **Thread 1 — Rulebase**: MYCIN base expanded from 6 → 15 rules with new
  fact types (`hospital-acquired`, `recent-travel`, `white-blood-count`,
  `infection-site`) and new demonstration functions (`culture-1a`,
  `culture-3`).
- **Bridge/driver wire-up for DS**: `LISA_BELIEF_SYSTEM` env var selects the
  system at bridge startup (**Dempster-Shafer is the default**); `POST
  /reset` accepts optional `belief_system` for per-session overrides;
  `/conclusions` emits `{bel, pl, ignorance}` under DS and bare numbers
  under CF; every response includes the active belief-system name.
- **Configurable session capture in the driver**: markdown transcripts land
  in `./sessions/` by default. CLI flags `--transcript` /
  `--no-transcript`, `--transcript-dir`, `--transcript-file`,
  `--transcript-verbosity {minimal,normal,full}`; matching env vars
  `LISA_TRANSCRIPT*`; runtime `transcript on|off|where` prompts.
- **Refreshed tool schemas + system prompt**: `tools.json` covers all 15
  rules' fact types and the `belief_system` param on reset;
  `system-prompt.md` narrates the CF vs DS output shapes and gives
  hedging guidance for wide DS ignorance intervals.
- **Clinician scenarios**: `docs/clinician-scenarios.md` — 7 vignettes
  covering every rule and highlighting the CF-vs-DS distinction.

- **User-facing runbook**: `docs/runbook.md` — narrative walkthrough covering
  bridge/driver startup, five hands-on demonstrations (belief combination,
  partial-matches-driven questioning, mid-session belief-system switching,
  DS ambiguous-evidence propagation, DS ignorance narrowing), transcript
  configuration, and a fact/rule reference.

### Remaining threads

- **Thread 3 — Architecture validation**: partially exercised by the 15-rule
  base; still worth deliberate stress testing with longer conversations and
  more competing hypotheses.

