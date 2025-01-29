<sub>_"Even in our sleep, pain which cannot forget falls drop by drop upon the heart, until, in our own despair, against our will, comes wisdom through the awful grace of God."_ -- Aeschylus<sub>

![_Always Remember_](images/powmia.png "Always Remember")

# Welcome to the Lisa Project #

Lisa is a production-quality, forward-chaining expert-system shell. The inference engine is an optimized, literal
implementation of Charles Forgy's Rete algorithm, a very efficient mechanism for solving the difficult many-to-many
matching problem[^1]. Lisa is written in modern Common Lisp, the Common Lisp Object System (CLOS), and the Meta Object
Protocol (MOP); it may be easily integrated into just about any Common Lisp application with little effort.

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

In December 2024, Lisa completed a re-home from SourceForge to GitHub. Just about every file was touched in some form,
and some aspects of the directory structure were reorganized. In addition, during my ten-year absence from Lisa it
appears a number of odd bugs had been introduced that required repair. I've done that (see [Credits](#credits)).

Several new features have been added to "modernize" Lisa (eg. logging support). Best of all, I've spent the past several
months profiling Lisa's behavior using the Slime/SBCL deterministic profiling interface. Several hot-spots were
discovered, and optimizations were done to these areas.

Lisa successfully loads and runs on SBCL 2.4.11, and the Monkey and Bananas test suite, a classic AI planning problem,
completes successfully:

_Some sample rules from examples/mab.lisp_:

```lisp
(defrule hold-chest-to-put-on-floor ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (on-top-of (not floor)) (weight light))
  (monkey (holding (not ?chest)))
  (not (goal-is-to (action hold) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?chest)
                      (argument-2 empty))))

(defrule put-chest-on-floor ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding ?chest)))
  (?thing (thing (name ?chest)))
  =>
  (format t "Monkey throws the ~A off the ~A onto the floor.~%" ?chest ?on)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor)))

(defrule get-key-to-unlock ()
  (goal-is-to (action unlock) (argument-1 ?obj))
  (thing (name ?obj) (on-top-of floor))
  (chest (name ?obj) (unlocked-by ?key))
  (monkey (holding (not ?key)))
  (not (goal-is-to (action hold) (argument-1 ?key)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?key)
                      (argument-2 empty))))
...
```

_Loading MAB, and a partial run output_:

```lisp
CL-USER> (load "examples/mab.lisp")
T
CL-USER> (in-package :lisa-mab)
LISA-MAB> #<PACKAGE "LISA-MAB">
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
...
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
dosage adjusted for patient's body weight. Lisa uses a forward-chaining version borrowed from Peter Norvig's excellent
book on artificial intelligence[^2]. The run output is brief, but the rulebase in _examples/mycin.lisp_ is an interesting
study, as it illustrates Lisa's implementation of Certainty Factors:

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

### Recent Features ###

- Support for Quicklisp. I've submitted Lisa for
  [inclusion](https://github.com/quicklisp/quicklisp-projects/issues/2469) in the Quicklisp project registry; just
  waiting to hear back. In the meantime, see _ql.lisp_ for installation details.
- Logger selected: [log4cl](https://github.com/7max/log4cl).
- Log messages inserted into strategic points, replacing format/error forms.
- Significant optimizations using Slime and SBCL's deterministic profiler.
- Fixed the long-broken TEST and LOGICAL conditional elements.
- Ported auto-notification to SBCL.

## Upcoming Plans ##

After taking an eleven-year hiatus, as of December 2024 I decided to resume work on Lisa, adding some modern features
and capabilities. The fundamental architecture will not change, as I'm quite happy with it. But, Lisa will get attention
with the following:

- Fully integrate Lisa with Quicklisp.
- Other minor ambitions yet to be determined.

**Note**: I've long considered adding additional conditional elements to Lisa, to bring it in line with the latest CLIPS
releases. However, after studying some of the unpleasant behavioral side effects, and reflecting on how seldom I might
have wanted an OR, FORALL, etc. CE when writing expert systems, I've decided against adopting them. I will stay within
Lisa's bounds of simplicity; IMHO these CEs are a syntactic convenience only, and not worth the trouble.

## Documentation ##

Please see the Lisa [Wiki page](https://github.com/youngde811/Lisa/wiki/Home) for complete details and documentation. In
particular, read the _Getting Started_ section first for details on using SBCL with Emacs.

**Note**: Lisa is currently being developed using SBCL only; no testing on other Lisp implementations is on the
schedule. However, the core code here represents Lisa version 3.2 as found on [Sourceforge](https://sourceforge.net/),
which should run properly on the Lisp implementations mentioned above. **A word of caution however**: folks NOT using
Emacs and SBCL are on their own at this time, until I'm able to begin regression testing with other Lisp environments.

## References ##

- The [SBCL](https://www.sbcl.org/) home page.
- [Emacs](https://emacsformacos.com/) for MacOS.
- [Peter Norvig](https://norvig.github.io/paip-lisp/#/).

## Credits ##

- _[gassechen](https://github.com/gassechen)_: Gaston has exercised and uncovered several Lisa bugs that must have crept
  in while I was absent for the past ten years. Thank you!
- _[cdmojoli](https://github.com/cdmojoli)_: Identified an issue with auto-notification and SBCL. Thank you!

## Author ##

[David E. Young](mailto://streetrod750@protonmail.com)

[^1]: "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem" Charles L. Forgy, Artificial Intelligence 19(1982), 17-37.
[^2]: "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp", Peter Norvig, 1991.
