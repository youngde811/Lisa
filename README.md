<sub>_"Even in our sleep, pain which cannot forget falls drop by drop upon the heart, until, in our own despair, against our will, comes wisdom through the awful grace of God."_ -- Aeschylus</sub>

![_Always Remember_](images/powmia.png "Always Remember")

# Lisa: A Common Lisp Expert System Shell

Lisa is a production-quality, forward-chaining expert system shell featuring an optimized implementation of Charles
Forgy's Rete algorithm, a highly efficient solution to the difficult many-to-many pattern matching problem[^1]. Written
in modern Common Lisp using the Common Lisp Object System (CLOS) and the Meta Object Protocol (MOP), Lisa is designed
for seamless integration with existing Common Lisp applications. Lisa maximizes the expressive power of CLOS and the MOP
without imposing strict class hierarchy constraints, allowing developers to add sophisticated reasoning capabilities to
their applications with minimal effort.

## Supported Implementations

Lisa is known to run on the following ANSI Common Lisp implementations:

- SBCL
- Clasp (**NEW!**)
- LispWorks
- Allegro Common Lisp (ACL)
- CLISP
- CMUCL (19a)
- OpenMCL
- Armed Bear Common Lisp (ABCL)

## Current Status

Lisa is under active development as of January 2025. In December 2024, the project was migrated from SourceForge to
GitHub, with comprehensive updates to the codebase and directory structure. Several bugs that had accumulated during the
project's hiatus were identified and resolved (see [Credits](#credits)).

Lisa has been modernized with new features including integrated logging support via
[log4cl](https://github.com/7max/log4cl). Most significantly, extensive performance profiling using SLIME and SBCL's
deterministic profiler has led to substantial optimization improvements (see [Performance
Optimization](#performance-optimization)).

### Examples

Lisa successfully loads and runs on SBCL 2.4.11. See the [Example Rulebases](./docs/ExampleRulebases.md) documentation
for demonstrations of classic AI problems implemented in Lisa, including the Monkey and Bananas planning problem, the
Towers of Hanoi, and others.

### Recent Features

- **Quicklisp Support**: As of Spring 2025.
- **Hybrid Symbolic + LLM Diagnostic Agent**: A full Claude-driven clinician
  interface backed by Lisa's Rete engine — three LLM backends (Anthropic API,
  CVS LMS/Hyperion, GCP Vertex), pluggable belief systems (certainty factors
  and Dempster-Shafer), configurable session transcripts, goal-directed
  dialogue via partial-match introspection, and an 18-rule expanded MYCIN
  rulebase (including disconfirming rules that make Dempster-Shafer's
  conflict handling visible). **Start here → [`docs/runbook.md`](docs/runbook.md).**
- **Pluggable Belief Systems**: Two algebras ship alongside the classic
  Shortliffe-Buchanan certainty factors: a Dempster-Shafer implementation
  carrying explicit `[bel, pl]` intervals — combining evidence via Dempster's
  rule of combination with conflict renormalization on the `{H, ¬H}` frame —
  and the protocol machinery to add more. Switch systems at bridge startup
  or per session via the LLM. See
  [`docs/clinician-scenarios.md`](docs/clinician-scenarios.md) for
  side-by-side CF vs DS behavior on the same case.
- **Modern Logging**: Integrated [log4cl](https://github.com/7max/log4cl) throughout the codebase, replacing ad-hoc format and error forms with structured logging.
- **Performance Optimization**: Runtime improvements through strategic inlining and build order optimization (see [Performance Optimization](#performance-optimization)).
- **Bug Fixes**: Repaired the long-broken TEST and LOGICAL conditional elements.
- **SBCL Enhancements**: Ported auto-notification support to SBCL.

## LLM Integration

Lisa ships with a fully working **hybrid symbolic + LLM diagnostic agent**:
Claude drives the natural-language conversation with the clinician; Lisa's
Rete engine handles deterministic forward-chaining inference with full rule
traceability and a pluggable belief algebra. The bridge between the two is
an optional ASDF system (`lisa-bridge`) built on
[Hunchentoot](https://edicl.github.io/hunchentoot/) and
[jzon](https://github.com/Zulu-Inuoe/jzon). Loading it is entirely opt-in
and does not affect the core system.

### Why This Is Interesting

LLMs are great at parsing free-form clinical narrative and producing
readable prose. They are also unreliable at multi-step deterministic
reasoning and cannot show you a rule trace. Symbolic expert systems like
Lisa are excellent at both — but historically required rigid input formats
no clinician would tolerate. Wire them together and each side does what
it's good at:

- **Claude** turns *"27-year-old burn patient, immunocompromised, blood
  culture shows aerobic gram-negative rods"* into structured fact
  assertions, uses `/partial-matches` to decide what to ask next, and
  narrates the results back in clinical prose — never inventing beliefs
  the payload doesn't contain.
- **Lisa** fires rules in a fixed order, combines beliefs across
  independent evidence paths, and produces a fully auditable conclusion
  naming the exact rules that led to it.
- **Dempster-Shafer** (the default belief system) reports beliefs as
  `{bel, pl, ignorance}` intervals, so the LLM can say "Pseudomonas at
  93% with only 7% residual uncertainty" instead of collapsing the
  uncertainty story to a single number.

### Try It in Ten Minutes

The [**runbook**](docs/runbook.md) walks through five hands-on
demonstrations — multi-rule belief combination, DS ignorance narrowing,
partial-match-driven questioning, mid-session belief-system switching,
and ambiguous evidence — with paste-ready clinician scripts and expected
outputs at every step. Companion doc: [`clinician-scenarios.md`](docs/clinician-scenarios.md)
catalogs seven annotated vignettes with expected differentials under
both belief systems.

### Architecture

```
  Clinician (natural language)
       |
       v
  Claude ---- tool calls (JSON) ----> Lisa Bridge (Hunchentoot)
       ^                                    |
       |                                    v
       +---- tool results (JSON) <----- Lisa Engine (Rete + pluggable belief)
                                            |
                                            v
                                  ./sessions/session-*.md
                                  (markdown transcript of every turn)
```

### Tools Exposed

| Endpoint | Purpose |
|----------|---------|
| `POST /assert-fact` | Assert a structured fact into working memory (with optional confidence) |
| `POST /run-inference` | Trigger forward-chaining inference |
| `GET /conclusions` | Retrieve inference results — payload shape follows the active belief system |
| `GET /rule-trace` | Retrieve which rules fired and their matching facts |
| `GET /partial-matches` | Query which rules are close to firing and what facts are still needed |
| `POST /reset` | Clear working memory; optionally switch belief system per session |

### Getting Started

Start the Lisa bridge from the SBCL REPL. Dempster-Shafer is the default
belief system; set `LISA_BELIEF_SYSTEM=cf` before launching SBCL for the
classic certainty-factor algebra.

```lisp
(ql:quickload :lisa-bridge)
(in-package :lisa-user)
(load "examples/mycin.lisp")     ; 18-rule expanded MYCIN base
(lisa-bridge:start)              ; default port 8090
```

Then run the clinician driver. Three LLM backends are auto-detected:

```bash
# Direct Anthropic API (default for public users)
export ANTHROPIC_API_KEY=...

# --- or --- CVS LMS / Hyperion (via `cvscode auth login`; auto-detected)

# --- or --- GCP Vertex AI:
# gcloud auth application-default login
# export ANTHROPIC_VERTEX_PROJECT_ID=your-gcp-project
# export CLOUD_ML_REGION=us-east5

pip install anthropic httpx rich   # rich renders Claude's markdown in the terminal
python src/llm/claude/driver.py
```

Session transcripts land in `./sessions/session-*.md` by default —
shareable, portable, review-friendly. See `docs/runbook.md` for the full
three-backend breakdown, transcript configuration, and belief-system
switching.

### Example: Diagnostic Conversation

Actual interaction between a clinician and Claude backed by Lisa's Rete
engine running under Dempster-Shafer. Claude extracts facts from the
free-form narrative, runs inference, and narrates the belief intervals —
then, when new evidence lands, produces a diff showing how each hypothesis
moved.

```
Clinician: I have a 27-year-old female burn patient. She's obviously
           immunocompromised from the burn. Blood culture from three days
           ago shows gram-negative rods, aerobic.

[Claude asserts: burn=serious, compromised-host=t (patient-1),
                 culture-site=blood, culture-age=3,
                 gram=neg, morphology=rod, aerobicity=aerobic (organism-1)]
[Calls run_inference -- 4 rules fire]
[Calls get_conclusions]

Assistant: Four rules fired on this evidence. Here's the differential:

  Organism            Belief   Ignorance   Rules
  Enterobacteriaceae  0.80     0.20        aerobic-gram-neg-rod-suggests-enterobacteriaceae
  Pseudomonas         0.76     0.24        (two rules combined: burn-patient + compromised-host)
  Klebsiella          0.50     0.50        aerobic-gram-neg-rod-in-compromised-host-suggests-klebsiella

Pseudomonas at 0.76 comes from *belief combination*: two independent rules
converge on the same organism, so DS combines them (0.4 + 0.6 with the
conservative combinator gives 0.76 with only 0.24 residual uncertainty).
Klebsiella is a real hit but only one moderate-belief rule supports it, so
DS honestly reports 50% supported / 50% still unresolved.

Would you like to add hospital-acquired status? That would fire two more
rules and tighten the Klebsiella interval considerably.

Clinician: Yes, add hospital-acquired.

[Claude asserts: hospital-acquired=t (patient-1)]
[Calls run_inference -- 6 rules fire now]
[Calls get_conclusions]

Assistant: Two additional rules fired. The differential has sharpened:

  Organism            Belief   Ignorance   Δ
  Pseudomonas         0.93     0.07        ↑ from 0.76 (three rules now combining)
  Enterobacteriaceae  0.80     0.20        --
  Klebsiella          0.80     0.20        ↑ from 0.50 (two rules now combining)

Pseudomonas is now the leading diagnosis at 0.93 with only 0.07 residual
uncertainty. Empiric coverage should prioritize antipseudomonal activity,
with attention to Enterobacteriaceae and Klebsiella as well -- which fits
standard practice for a nosocomial burn-unit bacteremia.
```

Notice what's happening: as evidence accumulates, DS's ignorance interval
*narrows* on well-supported hypotheses, and Claude produces a Δ column —
reasoning about what changed — without being asked to. One honest caveat:
every hypothesis above keeps `pl = 1.0`, because nothing in this case argues
*against* any organism. In that purely confirmatory regime DS's belief numbers
match what CF would report. The two algebras diverge the moment evidence
*conflicts* — which is exactly the next example.

### Example: Conflicting Evidence — Where CF and DS Part Ways

Give the engine *contradictory* evidence and Dempster-Shafer earns its keep.
Here the Gram stain itself is ambiguous, so the same organism has evidence
both for and against being gram-negative:

```
Clinician: Same 27-year-old burn patient, immunocompromised. Blood culture
           grew an anaerobic organism. But the Gram stain is ambiguous — the
           microbiologist is ~80% sure it's gram-negative rods, though it
           could be gram-positive, maybe 60% confidence.

[Claude asserts gram=neg (conf 0.8) AND gram=pos (conf 0.6), plus
                burn=serious, compromised-host=t, culture-site=blood,
                morphology=rod, aerobicity=anaerobic]
[Calls run_inference, get_conclusions -- belief system: Dempster-Shafer]

Assistant:
  Organism      bel    pl     ignorance
  Bacteroides   0.60   0.83   0.23
  Pseudomonas   0.51   0.80   0.29

Plausibility fell *below 1.0* here. The possible gram-positive reading fires
gram-pos-stain-argues-against-gram-neg-organism — evidence *against* every
gram-negative hypothesis. Under Dempster's rule of combination that conflict
renormalizes the interval: belief stays moderate, but the ceiling drops.

Clinician: Now run the identical case under certainty factors.

[Claude calls reset with belief_system=cf, re-asserts, re-runs]

Assistant:
  Organism      CF
  Bacteroides   0.52
  Pseudomonas   0.39

Same rules, same evidence — but CF collapses each hypothesis to one number.
You can't tell "well-supported but contested" (Bacteroides here) from
"weakly supported but uncontested"; both can land at the same CF. DS keeps
them apart: `bel` says how supported, `pl` says how contested, and `pl < 1.0`
is the visible fingerprint of the conflicting stain.
```

This whole exchange — driven live through Claude and the bridge under
`claude-sonnet-5` — is captured verbatim in
[`docs/sample-session-ds-conflict.md`](docs/sample-session-ds-conflict.md),
including the raw `{bel, pl, ignorance}` tool payloads.

### Goals

- Provide deterministic, auditable inference that an LLM cannot replicate on its own
- Keep Lisa's core unchanged — the bridge is purely additive
- Enable natural-language access to expert system reasoning without sacrificing explainability
- Make uncertainty visible: DS beliefs surface residual ignorance the LLM can narrate honestly
- Demonstrate that symbolic AI and neural approaches are complementary, not competing

## Performance Optimization

Recent profiling work using SBCL's deterministic profiler identified critical performance bottlenecks in Lisa's token
manipulation code. The primary hotspot was `TOKEN-PUSH-FACT`, which was being called millions of times during typical
inference runs without being inlined.

### Optimizations Applied

Several key optimizations were implemented:

1. **Inline Declarations**: Added `(declaim (inline ...))` declarations for hot-path token functions including
   `TOKEN-PUSH-FACT`, `TOKEN-POP-FACT`, and `TOKEN-TOP-FACT`.
   
2. **Accessor Replacement**: In a few cases, some hot class accessors were replaced with regular Lisp functions that now
   use `SLOT-VALUE`. Such rewrites allowed these functions to be inlined.

3. **Build Order Optimization**: Moved `token.lisp` earlier in the ASDF system definition (from 19th to 5th position) to
   ensure the compiler sees inline declarations before compiling files that depend on them. This simple reordering
   allowed the compiler to eliminate 45+ million function calls through inlining.

#### Results

**Time Improvements**

1. Critical hotspot (TEST-TOKENS)
   - Unoptimized: 1.298 seconds
   - Optimized: 0.629 seconds
   - 51% faster: cut execution time in half

2. Total profiled execution
   - Unoptimized: 3.298 seconds
   - Optimized: 1.614 seconds
   - 51% reduction in profiled time

**Memory Improvements**

1. Memory allocation reduction
   - Unoptimized: 13,612,824,256 bytes
   - Optimized: 8,609,908,064 bytes
   - 37% less memory allocated

#### Optimization Achievements

The key win is visible in the profile differences. In the unoptimized version, these hot functions appeared:
- FAST-ARRAY-COPY: 0.304s, 444MB consed
- TOKEN-PUSH-FACT: 0.301s, 356MB consed
- GET-SLOT-VALUE: 0.280s, 307MB consed
- TOKEN-TOP-FACT: 0.243s, 237MB consed
- TOKEN-HASH-CODE: 0.019s
- TOKEN-FACT-COUNT: 0.023s

In the optimized version, these functions show up in the "not called" list, confirming they've been successfully
inlined. Their overhead disappeared into their callers.

#### Summary

TEST-TOKENS dropped from 1.95GB consed to just 161MB consed - that's a 91% reduction in allocations for the hottest
function. The inlining eliminated function call overhead and allowed better compiler optimization.
  
These optimizations yielded a significant improvement in overall runtime performance on the Monkey and Bananas benchmark
(500 iterations). They demonstrate the importance of both compiler hints and compilation order in Common Lisp
systems. Lisa's CLOS-based architecture is **now performing close to its theoretical maximum on SBCL/ARM64**.

You can see the current Apple M2 Pro profiling benchmark results in the _sbcl_ directory.

### Roadmap

Lisa's fundamental CLOS-based architecture will remain unchanged, as it provides an elegant foundation for the Rete
implementation. Current development priorities include:

- **LLM Integration (Complete)**: HTTP bridge and Claude tool-use integration enabling natural-language access to
  Lisa's inference engine. Claude handles conversational fact-gathering; Lisa handles deterministic, explainable
  inference with certainty factors. See [LLM Integration](#llm-integration) above.

- **Linux Performance Testing**: Profiling will continue on x86-64 hardware to analyze Lisa's performance
  characteristics on SBCL/Linux. On that hardware, SBCL's statistical profiler should be available to better analyze
  Lisa's behavior under significant load.

### Development Status

Lisa is actively developed and tested on SBCL. Additional testing has been performed on
[AllegroExpress](https://franz.com/) (the free version of Allegro Common Lisp) with excellent results. The core codebase
represents Lisa version 4.2.0 and should run correctly on all supported Common Lisp implementations listed above.

### Design Philosophy

Lisa intentionally maintains a simpler feature set compared to some expert system shells like CLIPS. While additional
conditional elements (OR, FORALL, etc.) have been considered, they introduce behavioral complexity and are rarely
essential in practice. Lisa prioritizes simplicity and elegance over syntactic convenience, keeping the system
approachable and maintainable.

## Documentation

Complete documentation is available on the Lisa [Wiki](https://github.com/youngde811/Lisa/wiki/Home). New users should
start with the _Getting Started_ section for guidance on setting up SBCL with Emacs.

## References

- [SBCL](https://www.sbcl.org/) - Steel Bank Common Lisp
- [Emacs for MacOS](https://emacsformacos.com/)
- Peter Norvig's [Paradigms of Artificial Intelligence Programming](https://norvig.github.io/paip-lisp/#/)
- Expert Systems: Principles and Programming, Giarratano & Riley

## Acknowledgments

Over the years, several individuals have contributed enhancements, bug fixes, and useful functionality to Lisa. While
I've lost touch with many of them, their contributions deserve recognition:

- **Aneil Mallavarapu**: Contributed several enhancements and bug fixes to the core system.
- **Paolo Amoroso**: Early advocate for Lisa in the open source community who suggested adding Certainty Factor
  support. His [review](http://www.paoloamoroso.it/log/050827.html) of Lisa remains appreciated.
- **Paul Werkowski**: Contributed essential code for SBCL compatibility and an elegant package macro.
- **Fred Gilham**: Implemented auto-notification support for CMUCL.
- **[gassechen](https://github.com/gassechen)** (Gaston): Created [cl-lisa-web](https://github.com/gassechen/cl-lisa-web/tree/main), an impressive web-based front-end for Lisa.

## Recent Contributors

Special thanks to community members who have helped improve Lisa during its recent revival:

- **[gassechen](https://github.com/gassechen)** (Gaston): Extensively tested Lisa and identified several long-standing
  bugs that had accumulated during the project's hiatus.
- **[cdmojoli](https://github.com/cdmojoli)**: Identified and reported an auto-notification issue with SBCL.

## Author

[David E. Young](mailto:streetrod750@protonmail.com)

---

[^1]: Charles L. Forgy, "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem," _Artificial Intelligence_ 19 (1982): 17-37.
