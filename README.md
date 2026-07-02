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
- **LLM Integration**: Claude tool-use integration for natural-language access to Lisa's inference engine. See [LLM Integration](#llm-integration) below.
- **Modern Logging**: Integrated [log4cl](https://github.com/7max/log4cl) throughout the codebase, replacing ad-hoc format and error forms with structured logging.
- **Performance Optimization**: Runtime improvements through strategic inlining and build order optimization (see [Performance Optimization](#performance-optimization)).
- **Bug Fixes**: Repaired the long-broken TEST and LOGICAL conditional elements.
- **SBCL Enhancements**: Ported auto-notification support to SBCL.

## LLM Integration

Lisa includes an optional HTTP bridge and Claude tool-use integration that enables natural-language access to the
inference engine. This is a separate ASDF system (`lisa-bridge`) that depends on Lisa,
[Hunchentoot](https://edicl.github.io/hunchentoot/), and
[jzon](https://github.com/Zulu-Inuoe/jzon) — loading it is entirely opt-in and does not affect the core system.

### Architecture

The LLM handles natural-language interaction with the user (parsing free-text into structured facts, narrating
conclusions in plain language), while Lisa handles deterministic forward-chaining inference with full explainability. The
bridge translates between JSON (the LLM's native interchange format) and Lisa's CLOS-based fact representation.

```
  User (natural language)
       |
       v
  LLM (Claude) ---- tool calls (JSON) ----> Lisa Bridge (Hunchentoot)
       ^                                         |
       |                                         v
       +---- tool results (JSON) <---------- Lisa Engine (Rete)
```

### Tools Exposed

| Endpoint | Purpose |
|----------|---------|
| `POST /assert-fact` | Assert a structured fact into working memory |
| `POST /run-inference` | Trigger forward-chaining inference |
| `GET /conclusions` | Retrieve inference results with belief factors |
| `GET /rule-trace` | Retrieve which rules fired and their matching facts |
| `GET /partial-matches` | Query which rules are close to firing and what facts are still needed |
| `POST /reset` | Clear working memory for a new session |

### Getting Started

Start the Lisa bridge from the SBCL REPL:

```lisp
(ql:quickload :lisa-bridge)
(lisa-bridge:start)           ; default port 8090
(lisa-bridge:start :port 9000) ; or specify a port
```

Then run the clinician driver:

```bash
export ANTHROPIC_API_KEY=...
# Optional: point at an internal Anthropic-protocol wrapper
# export ANTHROPIC_BASE_URL=https://internal-wrapper.example.com
python src/llm/claude/driver.py
```

The initial domain is medical diagnosis using Lisa's existing Mycin rulebase (`examples/mycin.lisp`), demonstrating
certainty-factor-based reasoning with full rule-trace explainability.

### Example: Diagnostic Conversation

The following is an actual interaction between a clinician and Claude using Lisa's Mycin rulebase. Claude
parses natural language into structured facts, uses `/partial-matches` to determine what to ask next, then
runs inference and explains the results with full rule traceability.

```
Clinician: Blood culture shows gram-positive cocci.
Assistant: I'll record the facts from your blood culture observation.

[Asserts: culture-site=blood, gram=pos (organism-1), morphology=coccus (organism-1)]

Now let me check what additional information would help narrow the diagnosis.

[Calls get_partial_matches]

We're close to firing two rules, and both need the same piece of information:

- gram-pos-cocci-in-clumps-suggests-staphylococcus: If growth conformation is clumps -> Staphylococcus (belief 0.7)
- gram-pos-cocci-in-chains-suggests-streptococcus: If growth conformation is chains -> Streptococcus (belief 0.7)

Key question: What is the growth conformation on culture -- are the cocci
arranged in clumps (clusters) or chains?

Clinician: Clumps.
Assistant: Growth conformation recorded as clumps. That completes
gram-pos-cocci-in-clumps-suggests-staphylococcus.

[Asserts: growth-conformation=clumps (organism-1)]
[Calls run_inference]
[Calls get_conclusions]

Diagnosis:

  Organism         Certainty
  Staphylococcus   0.70 (70%)

Reasoning (gram-pos-cocci-in-clumps-suggests-staphylococcus):
  1. Gram stain: positive
  2. Morphology: coccus
  3. Growth conformation: clumps

This is a classic presentation -- gram-positive cocci growing in clusters is
the hallmark of Staphylococcus species. The 0.7 belief factor reflects the
rule's built-in confidence level.
```

### Goals

- Provide deterministic, auditable inference that an LLM cannot replicate on its own
- Keep Lisa's core unchanged -- the bridge is purely additive
- Enable natural-language access to expert system reasoning without sacrificing explainability
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
represents Lisa version 3.2 and should run correctly on all supported Common Lisp implementations listed above.

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
