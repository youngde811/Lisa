<sub>_"Even in our sleep, pain which cannot forget falls drop by drop upon the heart, until, in our own despair, against our will, comes wisdom through the awful grace of God."_ -- Aeschylus<sub>

# Welcome to the Lisa Project #

Lisa is a production-quality, forward-chaining expert-system shell, developed atop an optimized implementation of
Charles Forgy's Rete algorithm, a very efficient mechanism for solving the difficult many-to-many matching
problem[^1]. Lisa is written in modern Common Lisp and the Common Lisp Object System (CLOS), with a smattering of the
Meta Object Protocol (MOP) sprinkled about, and may be easily integrated into just about any Common Lisp application
with little effort.

A unique behavior of Lisa is the ability to reason over CLOS objects without imposing special class hierarchy
requirements; thus it should be possible to easily augment existing CLOS applications with reasoning capabilities. As
Lisa is an extension to Common Lisp, the full power of the Lisp environment is always available. Lisa-enabled
applications should run on any ANSI-compliant Common Lisp platform.

## Supported Lisps ##

Lisa is known to run on the following ANSI Common Lisp implementations:

- SBCL
- LispWorks
- Allegro Common Lisp (ACL)
- CLISP
- CMUCL (19a)
- OpenMCL
- Armed Bear Common Lisp (ABCL)

## Current Status ##

Lisa has just completed a re-home from SourceForge to GitHub. Just about every file was touched in some form, and some
aspects of the directory structure were reorganized.

Lisa successfully loads and runs on SBCL 2.4.11, and the Monkey and Bananas test suite, a classic AI planning problem,
completes successfully:

```lisp
CL-USER> (load "examples/mab.lisp")
T
CL-USER> (in-package :lisa-mab)
#<PACKAGE "LISA-MAB">
LISA-MAB> (run-mab)
<INFO> [15:00:26] lisa-mab mab.lisp (run-mab repeat-mab) - Starting run...
Monkey jumps off the GREEN-COUCH onto the floor.
Monkey walks to T2-2.
Monkey climbs onto the RED-COUCH.
Monkey climbs onto the BIG-PILLOW.
Monkey grabs the RED-CHEST.
Monkey throws the RED-CHEST off the BIG-PILLOW onto the floor.
Monkey jumps off the BIG-PILLOW onto the floor.
Monkey walks to T1-3.
Monkey grabs the RED-KEY.
Monkey walks to T2-2 holding the RED-KEY.
Monkey opens the RED-CHEST with the RED-KEY revealing the LADDER.
Monkey drops the RED-KEY.
Monkey climbs onto the RED-CHEST.
Monkey grabs the LADDER.
Monkey jumps off the RED-CHEST onto the floor.
Monkey walks to T7-7 holding the LADDER.
Monkey drops the LADDER.
Monkey climbs onto the LADDER.
Monkey grabs the BLUE-CHEST.
Monkey throws the BLUE-CHEST off the LADDER onto the floor.
Monkey jumps off the LADDER onto the floor.
Monkey grabs the LADDER.
Monkey walks to T8-8 holding the LADDER.
Monkey drops the LADDER.
Monkey climbs onto the LADDER.
Monkey grabs the GREEN-CHEST.
Monkey throws the GREEN-CHEST off the LADDER onto the floor.
Monkey jumps off the LADDER onto the floor.
Monkey walks to T2-2.
Monkey grabs the RED-KEY.
Monkey walks to T8-8 holding the RED-KEY.
Monkey opens the GREEN-CHEST with the RED-KEY revealing the BLUE-KEY.
Monkey drops the RED-KEY.
Monkey climbs onto the GREEN-CHEST.
Monkey grabs the BLUE-KEY.
Monkey jumps off the GREEN-CHEST onto the floor.
Monkey walks to T7-7 holding the BLUE-KEY.
Monkey opens the BLUE-CHEST with the BLUE-KEY revealing the BANANAS.
Monkey drops the BLUE-KEY.
Monkey climbs onto the BLUE-CHEST.
Monkey grabs the BANANAS.
Monkey eats the BANANAS.
Evaluation took:
  0.028 seconds of real time
  0.028730 seconds of total run time (0.027518 user, 0.001212 system)
  103.57% CPU
  241 lambdas converted
  13,727,952 bytes consed
  
NIL
```

Another interesting problem is MYCIN, an early backward chaining expert system that used artificial intelligence to
identify bacteria causing severe infections, such as bacteremia and meningitis, and to recommend antibiotics, with the
dosage adjusted for patient's body weight. Lisa uses a forward-chaining version borrowed from Peter Norvig's _"Paradigms
of Artificial Intelligence: Case Studies in Common Lisp"_. The run output is brief, but the rulebase in
_examples/mycin.lisp_ is an interesting study, as it illustrates Lisa's implementation of Certainty Factors:

```lisp
CL-USER> (load "examples/mycin")
T
CL-USER> (in-package :lisa-user)
#<PACKAGE "LISA-USER">
LISA-USER> (culture-1)
Identity: PSEUDOMONAS (0.760)
Identity: ENTEROBACTERIACEAE (0.800)
5
LISA-USER> (culture-2)
Identity: PSEUDOMONAS (0.646)
Identity: BACTEROIDES (0.720)
5
LISA-USER> 
```

## Upcoming Plans ##

After taking an eleven-year hiatus, as of December 2024 I've decided to resume work on Lisa, adding some modern features
and capabilities. The fundamental architecture will not change, as I'm quite happy with it. But, Lisa needs at least
some of the following:

- Re-introduction of the OR conditional element.
- More detailed performance analysis.
- Anything else I feel like adding.

### Completed Features ###

- Support for Quicklisp.
- Logger selected. We'll be using [log4cl](https://github.com/7max/log4cl).
- Log messages inserted into strategic points, replacing format/error forms.
- Significant optimizations using Slime and SBCL's deterministic profiler.
- Ported auto-notification to SBCL.

## Documentation ##

Please see the Lisa [Wiki page](https://github.com/youngde811/Lisa/wiki/Home) for complete details and documentation. In
particular, read the _Getting Started_ section first for details on using SBCL with Emacs.

**NB**: Lisa is currently being developed using SBCL only; no testing on other Lisp implementations is on the
schedule. However, the core code here represents Lisa version 3.2 as found on [Sourceforge](https://sourceforge.net/),
which should run properly on the Lisp implementations mentioned above. **A word of caution however**: folks NOT using
Emacs and SBCL are on their own at this time, until I'm able to begin regression testing with other Lisp environments.

## Author ##

[David E. Young](mailto://streetrod750@protonmail.com)

[^1]: "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem" Charles L. Forgy, Artificial Intelligence 19(1982), 17-37.
