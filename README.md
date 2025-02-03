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

Lisa successfully loads and runs on SBCL 2.4.11; see [here](./docs/ExampleRulebases.md) for a few examples of classic AI
problems runnable using Lisa.

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

**Note**: Lisa is currently being actively developed using SBCL only. However, minor testing has been performed using
[Allegro Express](https://franz.com/), the free version of Allegro Common Lisp, with perfect results. The core code here
represents Lisa version 3.2 as found on [Sourceforge](https://sourceforge.net/), which should run properly on the Lisp
implementations mentioned above.

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
