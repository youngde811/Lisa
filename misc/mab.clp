;;;======================================================
;;;   Monkees and Bananas Sample Problem
;;;
;;;     This is an extended version of a
;;;     rather common AI planning problem.
;;;     The point is for the monkee to find
;;;     and eat some bananas.
;;;
;;;     CLIPS Version 6.0 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================
;;;
;;; modified EJFH 4/11/96 for JES Java Expert System
;;; produces identical output under CLIPS

;;;(set-node-index-hash 1)
;;;(set-fact-duplication TRUE)
;;;(set-multithreaded-io TRUE)

;;;(watch facts)
;;;(watch activations)
;;;(watch rules)

(load-package jess.ViewFunctions)

;;;*************
;;;* TEMPLATES *
;;;*************

(deftemplate monkey 
   (slot location 
      (default green-couch))
   (slot on-top-of 
      (default floor)) 
   (slot holding 
      (default nothing)))

(deftemplate thing 
   (slot name)
   (slot location)
   (slot on-top-of 
      (default floor))
   (slot weight 
      (default light)))
                    
(deftemplate chest 
   (slot name)
   (slot contents)
   (slot unlocked-by))
               
(deftemplate goal-is-to 
   (slot action)
   (slot argument-1)
   (slot argument-2))
             
;;;*************************
;;;* CHEST UNLOCKING RULES *
;;;*************************

(defrule hold-chest-to-put-on-floor "" 
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (on-top-of ~floor) (weight light))
  (monkey (holding ~?chest))
  (not (goal-is-to (action hold) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?chest))))

(defrule put-chest-on-floor "" 
  (goal-is-to (action unlock) (argument-1 ?chest))
  ?monkey <- (monkey (location ?place) (on-top-of ?on) (holding ?chest))
  ?thing <- (thing (name ?chest))
  =>
  (printout t "Monkey throws the " ?chest " off the " 
              ?on " onto the floor." crlf)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor)))

(defrule get-key-to-unlock "" 
  (goal-is-to (action unlock) (argument-1 ?obj))
  (thing (name ?obj) (on-top-of floor))
  (chest (name ?obj) (unlocked-by ?key))
  (monkey (holding ~?key))
  (not (goal-is-to (action hold) (argument-1 ?key)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?key))))

;; one conjunction removed here just by reordering patterns.

(defrule move-to-chest-with-key "" 
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (location ?cplace) (on-top-of floor))
  (monkey (location ~?cplace) (holding ?key))
  (chest (name ?chest) (unlocked-by ?key))
  (not (goal-is-to (action walk-to) (argument-1 ?cplace)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?cplace))))

(defrule unlock-chest-with-key "" 
  ?goal <- (goal-is-to (action unlock) (argument-1 ?name))
  ?chest <- (chest (name ?name) (contents ?contents) (unlocked-by ?key))
  (thing (name ?name) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?on) (holding ?key))
  =>
  (printout t "Monkey opens the " ?name " with the " ?key 
              " revealing the " ?contents "." crlf)
  (modify ?chest (contents nothing))
  (assert (thing (name ?contents) (location ?place) (on-top-of ?name)))
  (retract ?goal))

;;;*********************
;;;* HOLD OBJECT RULES * 
;;;*********************

(defrule unlock-chest-to-hold-object ""
  (goal-is-to (action hold) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action unlock) (argument-1 ?chest))))

(defrule use-ladder-to-hold ""
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (not (thing (name ladder) (location ?place)))
  (not (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place)))
  =>
  (assert (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place))))

(defrule climb-ladder-to-hold ""
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (thing (name ladder) (location ?place) (on-top-of floor))
  (monkey (on-top-of ~ladder))
  (not (goal-is-to (action on) (argument-1 ladder)))
  =>
  (assert (goal-is-to (action on) (argument-1 ladder))))

(defrule grab-object-from-ladder "" 
  ?goal <- (goal-is-to (action hold) (argument-1 ?name))
  ?thing <- (thing (name ?name) (location ?place) 
                   (on-top-of ceiling) (weight light))
  (thing (name ladder) (location ?place))
  ?monkey <- (monkey (location ?place) (on-top-of ladder) (holding blank))
  =>
  (printout t "Monkey grabs the " ?name "." crlf)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule climb-to-hold ""
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place&~ceiling) (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of ~?on))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert (goal-is-to (action on) (argument-1 ?on))))

(defrule walk-to-hold ""
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ~ceiling) (weight light))
  (monkey (location ~?place))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place))))

(defrule drop-to-hold ""
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of ?on) (holding ~blank))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert (goal-is-to (action hold) (argument-1 blank))))

(defrule grab-object "" 
  ?goal <- (goal-is-to (action hold) (argument-1 ?name))
  ?thing <- (thing (name ?name) (location ?place) 
                   (on-top-of ?on) (weight light))
  ?monkey <- (monkey (location ?place) (on-top-of ?on) (holding blank))
  =>
  (printout t "Monkey grabs the " ?name "." crlf)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule drop-object ""  
  ?goal <- (goal-is-to (action hold) (argument-1 blank))
  ?monkey <- (monkey (location ?place) 
                     (on-top-of ?on) 
                     (holding ?name&~blank))
  ?thing <- (thing (name ?name))
  =>
  (printout t "Monkey drops the " ?name "." crlf)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of ?on))
  (retract ?goal))

;;;*********************
;;;* MOVE OBJECT RULES * 
;;;*********************

(defrule unlock-chest-to-move-object "" 
  (goal-is-to (action move) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action unlock) (argument-1 ?chest))))

(defrule hold-object-to-move ""  
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (thing (name ?obj) (location ~?place) (weight light))
  (monkey (holding ~?obj))
  (not (goal-is-to (action hold) (argument-1  ?obj)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?obj))))

(defrule move-object-to-place "" 
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (monkey (location ~?place) (holding ?obj))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place))))

(defrule drop-object-once-moved "" 
  ?goal <- (goal-is-to (action move) (argument-1 ?name) (argument-2 ?place))
  ?monkey <- (monkey (location ?place) (holding ?obj))
  ?thing <- (thing (name ?name) (weight light))
  =>
  (printout t "Monkey drops the " ?name "." crlf)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor))
  (retract ?goal))

(defrule already-moved-object ""
  ?goal <- (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (thing (name ?obj) (location ?place))
  =>
  (retract ?goal))

;;;***********************
;;;* WALK TO PLACE RULES *
;;;***********************

(defrule already-at-place "" 
  ?goal <- (goal-is-to (action walk-to) (argument-1 ?place))
  (monkey (location ?place))
  =>
  (retract ?goal))

(defrule get-on-floor-to-walk ""
  (goal-is-to (action walk-to) (argument-1 ?place))
  (monkey (location ~?place) (on-top-of ~floor))
  (not (goal-is-to (action on) (argument-1 floor)))
  =>
  (assert (goal-is-to (action on) (argument-1 floor))))

(defrule walk-holding-nothing ""
  ?goal <- (goal-is-to (action walk-to) (argument-1 ?place))
  ?monkey <- (monkey (location ~?place) (on-top-of floor) (holding blank))
  =>
  (printout t "Monkey walks to " ?place "." crlf)
  (modify ?monkey (location ?place))
  (retract ?goal))

(defrule walk-holding-object ""
  ?goal <- (goal-is-to (action walk-to) (argument-1 ?place))
  ?monkey <- (monkey (location ~?place) (on-top-of floor) (holding ?obj))
  (thing (name ?obj))
  =>
  (printout t "Monkey walks to " ?place " holding the " ?obj "." crlf)
  (modify ?monkey (location ?place))
  (retract ?goal))

;;;***********************
;;;* GET ON OBJECT RULES * 
;;;***********************

(defrule jump-onto-floor "" 
  ?goal <- (goal-is-to (action on) (argument-1 floor))
  ?monkey <- (monkey (on-top-of ?on&~floor))
  =>
  (printout t "Monkey jumps off the " ?on " onto the floor." crlf)
  (modify ?monkey (on-top-of floor))
  (retract ?goal))

(defrule walk-to-place-to-climb "" 
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location ~?place))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place))))

(defrule drop-to-climb "" 
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location ?place) (holding ~blank))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert (goal-is-to (action hold) (argument-1 blank))))

(defrule climb-indirectly "" 
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ~?on&~obj) (holding blank))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert (goal-is-to (action on) (argument-1 ?on))))

(defrule climb-directly ""  
  ?goal <- (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  ?monkey <- (monkey (location ?place) (on-top-of ?on) (holding blank))
  =>
  (printout t "Monkey climbs onto the " ?obj "." crlf)
  (modify ?monkey (on-top-of ?obj))
  (retract ?goal))

(defrule already-on-object ""
  ?goal <- (goal-is-to (action on) (argument-1 ?obj))
  (monkey (on-top-of ?obj))
  =>
  (retract ?goal))

;;;********************
;;;* EAT OBJECT RULES * 
;;;********************
(defrule hold-to-eat ""
  (goal-is-to (action eat) (argument-1 ?obj))
  (monkey (holding ~?obj))
  (not (goal-is-to (action hold) (argument-1 ?obj)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?obj))))

(defrule satisfy-hunger ""
  ?goal <- (goal-is-to (action eat) (argument-1 ?name))
  ?monkey <- (monkey (holding ?name))
  ?thing <- (thing (name ?name))
  =>
  (printout t "Monkey eats the " ?name "." crlf)
  (modify ?monkey (holding blank))
  (retract ?goal ?thing))
 
;;;**********************
;;;* INITIAL STATE RULE * 
;;;**********************

(defrule startup ""
  =>
  (assert (monkey (location t5-7) (on-top-of green-couch) (holding blank)))
  (assert (thing (name green-couch) (location t5-7) (weight heavy)))
  (assert (thing (name red-couch) (location t2-2) (weight heavy)))
  (assert (thing (name big-pillow) (location t2-2) (on-top-of red-couch)))
  (assert (thing (name red-chest) (location t2-2) (on-top-of big-pillow)))
  (assert (chest (name red-chest) (contents ladder) (unlocked-by red-key)))
  (assert (thing (name blue-chest) (location t7-7) (on-top-of ceiling)))
  (assert (thing (name grapes) (location t7-8) (on-top-of ceiling)))
  (assert (chest (name blue-chest) (contents bananas) (unlocked-by blue-key)))
  (assert (thing (name blue-couch) (location t8-8) (weight heavy)))
  (assert (thing (name green-chest) (location t8-8) (on-top-of ceiling)))
  (assert (chest (name green-chest) (contents blue-key) (unlocked-by red-key)))
  (assert (thing (name red-key) (location t1-3)))
  (assert (goal-is-to (action eat) (argument-1 bananas)))
  )

(defglobal ?*time* = (time))
(set-reset-globals FALSE)

(deffunction run-mab (?n)
  (bind ?start (time))
  (while (> ?n 0) do
         (reset)
         (run)
         (bind ?n (- ?n 1)))
  (printout t "Elapsed time: "
            (- (time) ?start) crlf))

(deffunction walk ()
  (run 1)
  (facts)
  (agenda))

;(reset)
;(run)

;;(run-n-times 256)
;;(run-n-times 1)
;;(printout t "Elapsed time: " (integer (- (time) ?*time*)) crlf)
