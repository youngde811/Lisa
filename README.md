
# Welcome to the Lisa Project #

Lisa is a production-quality expert-system shell, developed atop an optimized implementation of Charles Forgy's Rete
algorithm, a very efficient mechanism for solving the difficult many-to-many matching problem[^1]. Lisa is written in
modern Common Lisp and the Common Lisp Object System (CLOS), with a smattering of the Meta Object Protocol (MOP)
sprinkled about.

Intrinsic to Lisa is the ability to reason over CLOS objects without imposing special class hierarchy requirements; thus
it should be possible to easily augment existing CLOS applications with reasoning capabilities. As Lisa is an extension
to Common Lisp, the full power of the Lisp environment is always available. Lisa-enabled applications should run on any
ANSI-compliant Common Lisp platform.

Please see the Lisa [Wiki page](https://github.com/youngde811/Lisa/wiki/Lisa-Home) for complete details and documentation.

## Supported Platforms ##

Lisa is known to run on the following ANSI Common Lisp implementations:

- SBCL
- LispWorks
- ACL
- CLISP
- CMUCL (19a)
- OpenMCL
- Armed Bear Common Lisp (ABCL)

## Authors ##

[David E. Young](mailto://streetrod750@protonmail.com)

[^1]: "Rete: A Fast Algorithm for the Many Pattern/Many Object Pattern Match Problem" Charles L. Forgy, Artificial Intelligence 19(1982), 17-37.
