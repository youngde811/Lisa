
# Welcome to the Lisa Project #

Lisa is a production-quality expert-system shell, developed atop an optimized implementation of Charles Forgy's Rete
algorithm, a very efficient mechanism for solving the difficult many-to-many matching problem[^1]. Lisa is written in
modern Common Lisp and the Common Lisp Object System (CLOS), with a smattering of the Meta Object Protocol (MOP)
sprinkled about.

Intrinsic to Lisa is the ability to reason over CLOS objects without imposing special class hierarchy requirements; thus
it should be possible to easily augment existing CLOS applications with reasoning capabilities. As Lisa is an extension
to Common Lisp, the full power of the Lisp environment is always available. Lisa-enabled applications should run on any
ANSI-compliant Common Lisp platform.

## Project Goals ##

The Lisa project is driven by a number of important goals:

- _Unrestricted availability_: The principal motivation for beginning Lisa was the apparent dearth of lightweight,
  current, freely-available toolkits for constructing Lisp-based intelligent systems. There are a number of very fine
  commercial and restricted-use products out there (see the links below), but these systems do not necessarily address
  the needs of developers attempting to introduce Lisp into their organizations. This is exactly the position I found
  myself in prior to Lisa. I was leading an applied research project that needed an expert system shell, and if I'd had
  a modern implementation freely available for Lisp I might have been able to successfully entrench Lisp within the
  organization. So, I wrote Lisa.
  
- _Portability_: Vendor neutrality is a significant project goal, not because I disapprove of commercial software but to
  ensure Lisa is available on as many platforms as I can manage. As previously mentioned, Lisa is implemented in ANSI
  Common Lisp; currently, all development is being done using SBCL. Every effort will be made to avoid the use of
  implementation-specific functionality; in the cases where a non-ANSI feature must be used (e.g. multiprocessing
  support), Lisa will attempt to support the major Lisp implementations either through conditional evaluation or the use
  of a portable library.

- _Familiarity_: Lisa's production-rule system has its roots in CLIPS, making it readily familiar to developers who've
  worked with either that product, or something similar.

- _Flexibility_: Lisa reasons over CLOS objects without requiring changes to an application's class hierarchy. In
  addition, the full power of Common Lisp is available for use within productions; there is no dichotomy between Lisa's
  programming language and its implementation.

- _Simplicity_: Along with portability, the project strives for simplicity and elegance at the functional, architectural
  and source code levels. This doesn't mean Lisa should have limited usefulness or suffer from poor performance; rather,
  it should be possible for new developers to easily understand the code layout and behavior. Unless performance
  absolutely demands otherwise, clarity of design and implementation will take precedence.

[^1]: "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem" Charles L. Forgy, Artificial Intelligence 19(1982), 17-37.
