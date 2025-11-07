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
- **Modern Logging**: Integrated [log4cl](https://github.com/7max/log4cl) throughout the codebase, replacing ad-hoc format and error forms with structured logging.
- **Performance Optimization**: Runtime improvements through strategic inlining and build order optimization (see [Performance Optimization](#performance-optimization)).
- **Bug Fixes**: Repaired the long-broken TEST and LOGICAL conditional elements.
- **SBCL Enhancements**: Ported auto-notification support to SBCL.

## Performance Optimization

Recent profiling work using SBCL's deterministic profiler identified critical performance bottlenecks in Lisa's token
manipulation code. The primary hotspot was `TOKEN-PUSH-FACT`, which was being called over 45 million times during
typical inference runs without being inlined.

### Optimizations Applied

Two key optimizations were implemented:

1. **Inline Declarations**: Added `(declaim (inline ...))` declarations for hot-path token functions including `TOKEN-PUSH-FACT`, `TOKEN-POP-FACT`, and `TOKEN-TOP-FACT`.

2. **Build Order Optimization**: Moved `token.lisp` earlier in the ASDF system definition (from 19th to 5th position) to
   ensure the compiler sees inline declarations before compiling files that depend on them. This simple reordering
   allowed the compiler to eliminate 45+ million function calls through inlining.

### Results

- Time Improvements
  -  Critical hotspot (TEST-TOKENS):
     - Unoptimized: 1.298 seconds
     - Optimized: 0.629 seconds
     - 51% faster: cut execution time in half

  - Total profiled execution:
    - Unoptimized: 3.298 seconds
    - Optimized: 1.614 seconds
    - 51% reduction in profiled time

- Memory Improvements
  - Memory allocation reduction:
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

In optimized version, these functions show up in the "not called" list, confirming they've been successfully
inlined. Their overhead disappeared into their callers.

#### Summary

  TEST-TOKENS dropped from 1.95GB consed to just 161MB consed - that's a 91% reduction in allocations for the hottest
  function. The inlining eliminated function call overhead and allowed better compiler optimization.
  
These optimizations yielded a significant improvement in overall runtime performance on the Monkey and Bananas benchmark
(500 iterations). They demonstrate the importance of both compiler hints and compilation order in Common Lisp
systems. Lisa's CLOS-based architecture is now performing close to its theoretical maximum on SBCL/ARM64.

You can see the current Apple M2 Pro profiling benchmark results in the _sbcl_ directory.

### Future Work

Additional performance analysis using SBCL's statistical profiler on x86-64 Linux is planned to identify any remaining
optimization opportunities.

## Roadmap

Lisa's fundamental CLOS-based architecture will remain unchanged, as it provides an elegant foundation for the Rete
implementation. Current development priorities include:

- **Linux Performance Testing**: Profiling will continue on X86-64 hardware to analyze Lisa's performance characteristics on SBCL/Linux.
- **Statistical Profiling**: Conduct deeper performance analysis using SBCL's statistical profiler on the X86-64
  architecture to identify additional optimization opportunities.

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
