<sub>_"Even in our sleep, pain which cannot forget falls drop by drop upon the heart, until, in our own despair, against our will, comes wisdom through the awful grace of God."_ -- Aeschylus<sub>

![_Always Remember_](images/powmia.png "Always Remember")

# Welcome to the Lisa Project #

Lisa is a production-quality, forward-chaining expert-system shell. The inference engine is an optimized, literal
implementation of Charles Forgy's Rete algorithm, a very efficient mechanism for solving the difficult many-to-many
matching problem[^1]. Lisa is written in modern Common Lisp, the Common Lisp Object System (CLOS), and the Meta Object
Protocol (MOP). Designed for seamless integration with existing Common Lisp implementations, Lisa maximizes the
potential of CLOS and the MOP without enforcing strict class hierarchy limitations. This flexibility allows developers
to enhance their applications with sophisticated reasoning abilities with little effort.

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

### Examples ###

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

- Fully integrate Lisa with Quicklisp. I'm still awaiting confirmation on the PR I submitted for this work.
- Performance testing on Linux. Now that VirtualBox runs on ARM64 hardware, I'll be using a Linux VM to analyze how Lisa performs on SBCL/Linux. ArchLinux will be the distribution.
- Other minor ambitions yet to be determined.

While Lisa is currently being actively developed using SBCL, some testing has been performed using
[AllegroExpress](https://franz.com/) - the free version of Allegro Common Lisp - with perfect results. The core code here 
represents Lisa version 3.2 as found on [Sourceforge](https://sourceforge.net/), which should run properly on the Lisp
implementations mentioned above.

**Note**: I've long considered adding additional conditional elements to Lisa, to bring it in line with the latest CLIPS
releases. However, after studying some of the unpleasant behavioral side effects, and reflecting on how seldom I might
have wanted an OR, FORALL, etc. CE when writing expert systems, I've decided against adopting them. I will stay within
Lisa's bounds of simplicity; IMHO these CEs are a syntactic convenience only, and not worth the trouble.

## Documentation ##

Please see the Lisa [Wiki page](https://github.com/youngde811/Lisa/wiki/Home) for complete details and documentation. In
particular, read the _Getting Started_ section first for details on using SBCL with Emacs.

## References ##

- The [SBCL](https://www.sbcl.org/) home page.
- [Emacs](https://emacsformacos.com/) for MacOS.
- [Peter Norvig](https://norvig.github.io/paip-lisp/#/).
- [Expert Systems: Principles and Programming](https://www.powells.com/book/expert-systems-principles-programming-fourth-edition-principles-programming-9780534384470).

## Accolades ##

Over the years, serveral individuals have contributed to aspects of Lisa that either addressed bugs or added useful
functionality. I've lost touch with most of them, but they deserve mention here.

- Aneil Mallavarapu: Aneil contributed several enhancements and bug fixes.
- Paolo Amoroso: Paolo was a big fan of Lisa, and shared his thoughts within the Open Source community. He also gets
  credit for suggesting I add support for Certainty Factors. I thank him for his [review](http://www.paoloamoroso.it/log/050827.html).
- Paul Werkowski: Paul contributed code that helped get Lisa running on SBCL, and a very nice package macro.
- Fred Gilham: Fred contributed code to get auto-notification working on CMUCL.
- _[gassechen](https://github.com/gassechen)_: Gaston has written a very cool application that provides a UX front end to
  Lisa. Check it out [here](https://github.com/gassechen/cl-lisa-web/tree/main).

## Credits ##

- _[gassechen](https://github.com/gassechen)_: Gaston has exercised and uncovered several Lisa bugs that must have crept
  in while I was absent for the past ten years. Thank you!
- _[cdmojoli](https://github.com/cdmojoli)_: Identified an issue with auto-notification and SBCL. Thank you!

## Author ##

[David E. Young](mailto://streetrod750@protonmail.com)

[^1]: "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem" Charles L. Forgy, Artificial Intelligence 19(1982), 17-37.
