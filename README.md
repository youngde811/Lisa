
# Welcome to the Lisa Project #

Lisa is a production-quality, forward-chaining expert-system shell, developed atop an optimized implementation of
Charles Forgy's Rete algorithm, a very efficient mechanism for solving the difficult many-to-many matching
problem[^1]. Lisa is written in modern Common Lisp and the Common Lisp Object System (CLOS), with a smattering of the
Meta Object Protocol (MOP) sprinkled about, and may be easily integrated into just about any Common Lisp application
with little effort.

A core behavior of Lisa is the ability to reason over CLOS objects without imposing special class hierarchy
requirements; thus it should be possible to easily augment existing CLOS applications with reasoning capabilities. As
Lisa is an extension to Common Lisp, the full power of the Lisp environment is always available. Lisa-enabled
applications should run on any ANSI-compliant Common Lisp platform.

**NB**: Lisa is currently undergoing a re-home from SourceForge to GitHub (here). Just about every file has been touched
in some way, and the code itself has not yet been regression tested in SBCL. Once done (soon), I'll remove this notice.

## Supported Lisps ##

Lisa is known to run on the following ANSI Common Lisp implementations:

- SBCL
- LispWorks
- Allegro Common Lisp (ACL)
- CLISP (apparently no longer maintained)
- CMUCL (19a)
- OpenMCL
- Armed Bear Common Lisp (ABCL)

## What's New ##

After taking an eleven-year hiatus, as of December 2024 I've decided to resume work on Lisa, adding some modern features
and capabilities. The fundamental architecture will not change, as I'm quite happy with it. But, Lisa needs at least
some of the following:

- A flexible and configurable logging system; something akin to Log4J perhaps.
- A non-invasive metrics-gathering system for instrumentation. OpenTelemetry will not be considered, as it has zero
  support for Common Lisp.
- Project re-homing from Sourceforge to GitHub, including codebase cleanup and improved document presentation.
- Performance tuning using SBCL's profiler, locating hotspots in any areas of the code (particularly the inference
  engine and compiler).
- Support for Quicklisp.
- Anything else I feel like adding.

## Documentation ##

Please see the Lisa [Wiki page](https://github.com/youngde811/Lisa/wiki/Home) for complete details, features and
documentation.

## Authors ##

[David E. Young](mailto://streetrod750@protonmail.com)

"Even in our sleep, pain which cannot forget falls drop by drop upon the heart, until, in our own despair, against our will, comes wisdom through the awful grace of God."
-- Aeschylus

[^1]: "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem" Charles L. Forgy, Artificial Intelligence 19(1982), 17-37.
