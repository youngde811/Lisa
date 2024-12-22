;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.

;; MIT License

;; Copyright (c) 2000 David Young

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Description: The "Monkey And Bananas" sample implementation, a common AI
;; planning problem. The monkey's objective is to find and eat some bananas.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package "LISA-MAB"))
    (defpackage "LISA-MAB"
      (:use "LISA-LISP")
      (:export "RUN-MAB"))))

(in-package "LISA-MAB")

(deftemplate monkey ()
  (slot location)
  (slot on-top-of)
  (slot holding))

(deftemplate thing ()
  (slot name)
  (slot location)
  (slot on-top-of)
  (slot weight))

(deftemplate chest ()
  (slot name)
  (slot contents)
  (slot unlocked-by))

(deftemplate goal-is-to ()
  (slot action)
  (slot argument-1)
  (slot argument-2))

;;;(watch :activations)
;;;(watch :facts)
;;;(watch :rules)

;;; Chest-unlocking rules...

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

(defrule move-to-chest-with-key ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (location ?cplace) (on-top-of floor))
  (monkey (location (not ?cplace)) (holding ?key))
  (chest (name ?chest) (unlocked-by ?key))
  (not (goal-is-to (action walk-to) (argument-1 ?cplace)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?cplace)
                      (argument-2 empty))))

(defrule unlock-chest-with-key ()
  (?goal (goal-is-to (action unlock) (argument-1 ?name)))
  (?chest (chest (name ?name) (contents ?contents) (unlocked-by ?key)))
  (thing (name ?name) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?on) (holding ?key))
  =>
  (format t "Monkey opens the ~A with the ~A revealing the ~A.~%"
          ?name ?key ?contents)
  (modify ?chest (contents nothing))
  (assert (thing (name ?contents) (location ?place) 
                 (weight light) (on-top-of ?name)))
  (retract ?goal))

;;; Hold-object rules...

(defrule unlock-chest-to-hold-object ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action unlock) (argument-1 ?chest)
                      (argument-2 empty))))

(defrule use-ladder-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (not (thing (name ladder) (location ?place)))
  (not (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place)))
  =>
  (assert (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place))))

(defrule climb-ladder-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (thing (name ladder) (location ?place) (on-top-of floor))
  (monkey (on-top-of (not ladder)))
  (not (goal-is-to (action on) (argument-1 ladder)))
  =>
  (assert (goal-is-to (action on) (argument-1 ladder)
                      (argument-2 empty))))

(defrule grab-object-from-ladder ()
  (?goal (goal-is-to (action hold) (argument-1 ?name)))
  (?thing (thing (name ?name) (location ?place) 
                 (on-top-of ceiling) (weight light)))
  (thing (name ladder) (location ?place))
  (?monkey (monkey (location ?place) (on-top-of ladder) (holding blank)))
  =>
  (format t "Monkey grabs the ~A.~%" ?name)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule climb-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place (not ceiling))
         (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of (not ?on)))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert (goal-is-to (action on) (argument-1 ?on)
                      (argument-2 empty))))

(defrule walk-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of (not ceiling))
         (weight light))
  (monkey (location (not ?place)))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place)
                      (argument-2 empty))))

(defrule drop-to-hold ()
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of ?on) (holding (not blank)))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert (goal-is-to (action hold) (argument-1 blank)
                      (argument-2 empty))))

(defrule grab-object ()
  (?goal (goal-is-to (action hold) (argument-1 ?name)))
  (?thing (thing (name ?name) (location ?place) 
                 (on-top-of ?on) (weight light)))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding blank)))
  =>
  (format t "Monkey grabs the ~A.~%" ?name)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule drop-object ()
  (?goal (goal-is-to (action hold) (argument-1 blank)))
  (?monkey (monkey (location ?place) (on-top-of ?on) 
                   (holding ?name (not blank))))
  (?thing (thing (name ?name)))
  =>
  (format t "Monkey drops the ~A.~%" ?name)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of ?on))
  (retract ?goal))

;;; Move-object rules...

(defrule unlock-chest-to-move-object ()
  (goal-is-to (action move) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action unlock) (argument-1 ?chest)
                      (argument-2 empty))))

(defrule hold-object-to-move ()
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (thing (name ?obj) (location (not ?place)) (weight light))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1  ?obj)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?obj)
                      (argument-2 empty))))

(defrule move-object-to-place ()
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (monkey (location (not ?place)) (holding ?obj))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place)
                      (argument-2 empty))))

(defrule drop-object-once-moved ()
  (?goal (goal-is-to (action move) (argument-1 ?name) (argument-2 ?place)))
  (?monkey (monkey (location ?place) (holding ?obj)))
  (?thing (thing (name ?name) (weight light)))
  =>
  (format t "Monkey drops the ~A.~%" ?name)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor))
  (retract ?goal))

(defrule already-moved-object ()
  (?goal (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place)))
  (thing (name ?obj) (location ?place))
  =>
  (retract ?goal))

;;; Walk-to-place rules...

(defrule already-at-place ()
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (monkey (location ?place))
  =>
  (retract ?goal))

(defrule get-on-floor-to-walk ()
  (goal-is-to (action walk-to) (argument-1 ?place))
  (monkey (location (not ?place)) (on-top-of (not floor)))
  (not (goal-is-to (action on) (argument-1 floor)))
  =>
  (assert (goal-is-to (action on) (argument-1 floor)
                      (argument-2 empty))))

(defrule walk-holding-nothing ()
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (?monkey (monkey (location (not ?place)) (on-top-of floor) (holding blank)))
  =>
  (format t "Monkey walks to ~A.~%" ?place)
  (modify ?monkey (location ?place))
  (retract ?goal))

(defrule walk-holding-object ()
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (?monkey (monkey (location (not ?place)) (on-top-of floor) (holding ?obj)))
  (thing (name ?obj))
  =>
  (format t "Monkey walks to ~A holding the ~A.~%" ?place ?obj)
  (modify ?monkey (location ?place))
  (retract ?goal))

;;; Get-on-object rules...

(defrule jump-onto-floor ()
  (?goal (goal-is-to (action on) (argument-1 floor)))
  (?monkey (monkey (on-top-of ?on (not floor))))
  =>
  (format t "Monkey jumps off the ~A onto the floor.~%" ?on)
  (modify ?monkey (on-top-of floor))
  (retract ?goal))

(defrule walk-to-place-to-climb ()
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location (not ?place)))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place)
                      (argument-2 empty))))

(defrule drop-to-climb ()
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location ?place) (holding (not blank)))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert (goal-is-to (action hold) (argument-1 blank)
                      (argument-2 empty))))

(defrule climb-indirectly ()
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?top
                                       (and (not (eq ?top ?on))
                                            (not (eq ?top ?obj))))
          (holding blank))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert (goal-is-to (action on) (argument-1 ?on)
                      (argument-2 empty))))

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

;;; startup rule...

#+ignore
(defrule startup ()
  =>
  (assert (monkey (location t5-7) (on-top-of green-couch)
                  (location green-couch) (holding blank)))
  (assert (thing (name green-couch) (location t5-7) (weight heavy)
                 (on-top-of floor)))
  (assert (thing (name red-couch) (location t2-2) 
                 (on-top-of floor) (weight heavy)))
  (assert (thing (name big-pillow) (location t2-2) 
                 (weight light) (on-top-of red-couch)))
  (assert (thing (name red-chest) (location t2-2) 
                 (weight light) (on-top-of big-pillow)))
  (assert (chest (name red-chest) (contents ladder) (unlocked-by red-key)))
  (assert (thing (name blue-chest) (location t7-7) 
                 (weight light) (on-top-of ceiling)))
  (assert (thing (name grapes) (location t7-8) 
                 (weight light) (on-top-of ceiling)))
  (assert (chest (name blue-chest) (contents bananas) (unlocked-by blue-key)))
  (assert (thing (name blue-couch) (location t8-8) 
                 (on-top-of floor) (weight heavy)))
  (assert (thing (name green-chest) (location t8-8) 
                 (weight light) (on-top-of ceiling)))
  (assert (chest (name green-chest) (contents blue-key) (unlocked-by red-key)))
  (assert (thing (name red-key) 
                 (on-top-of floor) (weight light) (location t1-3)))
  (assert (goal-is-to (action eat) (argument-1 bananas) (argument-2 empty))))

(deffacts mab-startup ()
  (monkey (location t5-7) (on-top-of green-couch)
          (location green-couch) (holding blank))
  (thing (name green-couch) (location t5-7) (weight heavy)
         (on-top-of floor))
  (thing (name red-couch) (location t2-2) 
         (on-top-of floor) (weight heavy))
  (thing (name big-pillow) (location t2-2) 
         (weight light) (on-top-of red-couch))
  (thing (name red-chest) (location t2-2) 
         (weight light) (on-top-of big-pillow))
  (chest (name red-chest) (contents ladder) (unlocked-by red-key))
  (thing (name blue-chest) (location t7-7) 
         (weight light) (on-top-of ceiling))
  (thing (name grapes) (location t7-8) 
         (weight light) (on-top-of ceiling))
  (chest (name blue-chest) (contents bananas) (unlocked-by blue-key))
  (thing (name blue-couch) (location t8-8) 
         (on-top-of floor) (weight heavy))
  (thing (name green-chest) (location t8-8) 
         (weight light) (on-top-of ceiling))
  (chest (name green-chest) (contents blue-key) (unlocked-by red-key))
  (thing (name red-key) 
         (on-top-of floor) (weight light) (location t1-3))
  (goal-is-to (action eat) (argument-1 bananas) (argument-2 empty)))

#+ignore
(defun run-mab (&optional (ntimes 1))
  (let ((start (get-internal-real-time)))
    (dotimes (i ntimes)
      (format t "Starting run.~%")
      (reset)
      (run))
    (format t "Elapsed time: ~F~%"
            (/ (- (get-internal-real-time) start)
               internal-time-units-per-second))))

(defun run-mab (&optional (ntimes 1))
  (flet ((repeat-mab ()
           (dotimes (i ntimes)
             (log:info "Starting run...")
             (reset)
             (run))))
    (time (repeat-mab))))

#+Allegro
(defun profile-mab (&optional (ntimes 10))
  (prof:with-profiling (:type :time)
    (run-mab ntimes)))

#+LispWorks
(defun profile-mab (&optional (ntimes 10))
  (hcl:set-up-profiler :packages '("LISA" "CLOS"))
  (hcl:profile (run-mab ntimes)))
