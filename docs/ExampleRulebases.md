## The Monkey and Banana (MAB) Problem ##

The Monkey and Banana[^1] problem is a classic artificial intelligence planning/logic problem that has been solved using
various expert system shells and logic programming languages. Lisa's implementation is a direct translation of the
source implementation written for CLIPS[^2]; it may be found in [_examples/mab.lisp_](./examples/mab.lisp) and
[_examples/mab-clos.lisp_](./examples/mab-clos.lisp).

### Sample MAB Code ###

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

(defrule climb-directly ()
  (?goal (goal-is-to (action on) (argument-1 ?obj)))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding blank)))
  =>
  (format t "Monkey climbs onto the ~A.~%" ?obj)
  (modify ?monkey (on-top-of ?obj))
  (retract ?goal))

(defrule already-on-object ()
  (?goal (goal-is-to (action on) (argument-1 ?obj)))
  (monkey (on-top-of ?obj))
  =>
  (retract ?goal))

;;; Eat-object rules...

(defrule hold-to-eat ()
  (goal-is-to (action eat) (argument-1 ?obj))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1 ?obj)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?obj)
                      (argument-2 empty))))

(defrule satisfy-hunger ()
  (?goal (goal-is-to (action eat) (argument-1 ?name)))
  (?monkey (monkey (holding ?name)))
  (?thing (thing (name ?name)))
  =>
  (format t "Monkey eats the ~A.~%" ?name)
  (modify ?monkey (holding blank))
  (retract ?goal)
  (retract ?thing))
```

### Sample MAB Run ###

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

## MYCIN ##

Another interesting problem is MYCIN, an early backward chaining expert system that used artificial intelligence to
identify bacteria causing severe infections, such as bacteremia and meningitis, and to recommend antibiotics, with the
dosage adjusted for patient's body weight. Lisa uses a forward-chaining version borrowed from Peter Norvig's excellent
book on artificial intelligence[^3]. The run output is brief, but the rulebase in
[_examples/mycin.lisp_](./examples/mycin.lisp] is an interesting study, as it illustrates Lisa's implementation of
Certainty Factors.

### Sample MYCIN Code ###

```lisp
(defrule rule-52 (:belief 0.4)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (burn (value serious))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule rule-71 (:belief 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value clumps))
  =>
  (assert (organism-identity (value staphylococcus) (entity ?organism))))

(defrule rule-73 (:belief 0.9)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (aerobicity (value anaerobic))
  =>
  (assert (organism-identity (value bacteroides) (entity ?organism))))
  
...
```

### Sample MYCIN Run ###

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

[^1]: The [Monkey and Banana](https://en.wikipedia.org/wiki/Monkey_and_banana_problem) AI problem.
[^2]: [CLIPS](https://www.clipsrules.net/): A tool for building expert systems.
[^3]: "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp", Peter Norvig, 1991.
