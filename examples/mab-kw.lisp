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

(require "kw")

(in-package "KW-USER")

(def-kb-struct monkey
    location on-top-of holding)

(def-kb-struct thing
    name location on-top-of weight)

(def-kb-struct chest
    name contents unlocked-by)

(def-kb-struct goal-is-to
    action argument-1 argument-2)

(defcontext mab :strategy (priority recency order) :auto-return t)

;;; Chest-unlocking rules...

(defrule hold-chest-to-put-on-floor :forward :context mab
  (goal-is-to ? action unlock argument-1 ?chest)
  (thing ? name ?chest on-top-of ?top weight light)
  (test (not (eql ?top 'floor)))
  (not (monkey ? holding ?chest))
  (not (goal-is-to ? action hold argument-1 ?chest))
  -->
  (assert (goal-is-to ? action hold argument-1 ?chest)))

(defrule put-chest-on-floor :forward :context mab
  (goal-is-to ? action unlock argument-1 ?chest)
  (monkey ?monkey location ?place on-top-of ?on holding ?chest)
  (thing ?thing name ?chest)
  -->
  ((format t "Monkey throws the ~A off the ~A onto the floor.~%" ?chest ?on))
  (assert (monkey ?monkey holding blank))
  (assert (thing ?thing location ?place on-top-of floor)))

(defrule get-key-to-unlock :forward :context mab
  (goal-is-to ? action unlock argument-1 ?obj)
  (thing ? name ?obj on-top-of floor)
  (chest ? name ?obj unlocked-by ?key)
  (monkey ? holding ?hold)
  (test (not (eql ?hold ?key)))
  (not (goal-is-to ? action hold argument-1 ?key))
  -->
  (assert (goal-is-to ? action hold argument-1 ?key)))

(defrule move-to-chest-with-key :forward :context mab
  (goal-is-to ? action unlock argument-1 ?chest)
  (thing ? name ?chest location ?cplace on-top-of floor)
  (monkey ? location ?loc holding ?key)
  (test (not (eql ?loc ?cplace)))
  (chest ? name ?chest unlocked-by ?key)
  (not (goal-is-to ? action walk-to argument-1 ?cplace))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?cplace)))

(defrule unlock-chest-with-key :forward :context mab
  (goal-is-to ?goal action unlock argument-1 ?name)
  (chest ?chest name ?name contents ?contents unlocked-by ?key)
  (thing ? name ?name location ?place on-top-of ?on)
  (monkey ? location ?place on-top-of ?on holding ?key)
  -->
  ((format t "Monkey opens the ~A with the ~A revealing the ~A.~%"
           ?name ?key ?contents))
  (assert (chest ?chest contents nothing))
  (assert (thing ? name ?contents location ?place weight light
                 on-top-of ?name))
  (erase ?goal))

;;; Hold-object rules...

(defrule unlock-chest-to-hold-object :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (chest ? name ?chest contents ?obj)
  (not (goal-is-to ? action unlock argument-1 ?chest))
  -->
  (assert (goal-is-to ? action unlock argument-1 ?chest)))

(defrule use-ladder-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ceiling weight light)
  (not (thing ? name ladder location ?place))
  (not (goal-is-to ? action move argument-1 ladder argument-2 ?place))
  -->
  (assert (goal-is-to ? action move argument-1 ladder argument-2 ?place)))

(defrule climb-ladder-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ceiling weight light)
  (thing ? name ladder location ?place on-top-of floor)
  (monkey ? on-top-of ?top)
  (test (not (eql ?top 'ladder)))
  (not (goal-is-to ? action on argument-1 ladder))
  -->
  (assert (goal-is-to ? action on argument-1 ladder)))

(defrule grab-object-from-ladder :forward :context mab
  (goal-is-to ?goal action hold argument-1 ?name)
  (thing ?thing name ?name location ?place 
         on-top-of ceiling weight light)
  (thing ? name ladder location ?place)
  (monkey ?monkey location ?place on-top-of ladder holding blank)
  -->
  ((format t "Monkey grabs the ~A.~%" ?name))
  (assert (thing ?thing location held on-top-of held))
  (assert (monkey ?monkey holding ?name))
  (erase ?goal))

(defrule climb-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on
         weight light)
  (test (not (eql ?place 'ceiling)))
  (monkey ? location ?place on-top-of ?top)
  (test (not (eql ?top ?on)))
  (not (goal-is-to ? action on argument-1 ?on))
  -->
  (assert (goal-is-to ? action on argument-1 ?on)))

(defrule walk-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?top weight light)
  (test (not (eql ?top 'ceiling)))
  (monkey ? location ?loc)
  (test (not (eql ?loc ?place)))
  (not (goal-is-to ? action walk-to argument-1 ?place))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?place)))

(defrule drop-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on weight light)
  (monkey ? location ?place on-top-of ?on holding ?hold)
  (test (not (eql ?hold 'blank)))
  (not (goal-is-to ? action hold argument-1 blank))
  -->
  (assert (goal-is-to ? action hold argument-1 blank)))

(defrule grab-object :forward :context mab
  (goal-is-to ?goal action hold argument-1 ?name)
  (thing ?thing name ?name location ?place 
                 on-top-of ?on weight light)
  (monkey ?monkey location ?place on-top-of ?on holding blank)
  -->
  ((format t "Monkey grabs the ~A.~%" ?name))
  (assert (thing ?thing location held on-top-of held))
  (assert (monkey ?monkey holding ?name))
  (erase ?goal))

(defrule drop-object :forward :context mab
  (goal-is-to ?goal action hold argument-1 blank)
  (monkey ?monkey location ?place on-top-of ?on
          holding ?name)
  (test (not (eql ?name 'blank)))
  (thing ?thing name ?name)
  -->
  ((format t "Monkey drops the ~A.~%" ?name))
  (assert (monkey ?monkey holding blank))
  (assert (thing ?thing location ?place on-top-of ?on))
  (erase ?goal))

;;; Move-object rules...

(defrule unlock-chest-to-move-object :forward :context mab
  (goal-is-to ? action move argument-1 ?obj)
  (chest ? name ?chest contents ?obj)
  (not (goal-is-to ? action unlock argument-1 ?chest))
  -->
  (assert (goal-is-to ? action unlock argument-1 ?chest)))

(defrule hold-object-to-move :forward :context mab
  (goal-is-to ? action move argument-1 ?obj argument-2 ?place)
  (thing ? name ?obj location ?loc weight light)
  (test (not (eql ?loc ?place)))
  (monkey ? holding ?hold)
  (test (not (eql ?hold ?obj)))
  (not (goal-is-to ? action hold argument-1  ?obj))
  -->
  (assert (goal-is-to ? action hold argument-1 ?obj)))

(defrule move-object-to-place :forward :context mab
  (goal-is-to ? action move argument-1 ?obj argument-2 ?place)
  (monkey ? location ?loc holding ?obj)
  (test (not (eql ?loc ?place)))
  (not (goal-is-to ? action walk-to argument-1 ?place))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?place)))

(defrule drop-object-once-moved :forward :context mab
  (goal-is-to ?goal action move argument-1 ?name argument-2 ?place)
  (monkey ?monkey location ?place holding ?obj)
  (thing ?thing name ?name weight light)
  -->
  ((format t "Monkey drops the ~A.~%" ?name))
  (assert (monkey ?monkey holding blank))
  (assert (thing ?thing location ?place on-top-of floor))
  (erase ?goal))

(defrule already-moved-object :forward :context mab
  (goal-is-to ?goal action move argument-1 ?obj argument-2 ?place)
  (thing ? name ?obj location ?place)
  -->
  (erase ?goal))

;;; Walk-to-place rules...

(defrule already-at-place :forward :context mab
  (goal-is-to ?goal action walk-to argument-1 ?place)
  (monkey ? location ?place)
  -->
  (erase ?goal))

(defrule get-on-floor-to-walk :forward :context mab
  (goal-is-to ? action walk-to argument-1 ?place)
  (monkey ? location ?loc on-top-of ?on)
  (test (and (not (eql ?loc ?place))
             (not (eql ?on 'floor))))
  (not (goal-is-to ? action on argument-1 floor))
  -->
  (assert (goal-is-to ? action on argument-1 floor)))

(defrule walk-holding-nothing :forward :context mab
  (goal-is-to ?goal action walk-to argument-1 ?place)
  (monkey ?monkey location ?loc on-top-of floor holding blank)
  (test (not (eql ?loc ?place)))
  -->
  ((format t "Monkey walks to ~A.~%" ?place))
  (assert (monkey ?monkey location ?place))
  (erase ?goal))

(defrule walk-holding-object :forward :context mab
  (goal-is-to ?goal action walk-to argument-1 ?place)
  (monkey ?monkey location ?loc on-top-of floor holding ?obj)
  (test (not (eql ?loc ?place)))
  (thing ? name ?obj)
  -->
  ((format t "Monkey walks to ~A holding the ~A.~%" ?place ?obj))
  (assert (monkey ?monkey location ?place))
  (erase ?goal))

;;; Get-on-object rules...

(defrule jump-onto-floor :forward :context mab
  (goal-is-to ?goal action on argument-1 floor)
  (monkey ?monkey on-top-of ?on)
  (test (not (eql ?on 'floor)))
  -->
  ((format t "Monkey jumps off the ~A onto the floor.~%" ?on))
  (assert (monkey ?monkey on-top-of floor))
  (erase ?goal))

(defrule walk-to-place-to-climb :forward :context mab
  (goal-is-to ? action on argument-1 ?obj)
  (thing ? name ?obj location ?place)
  (monkey ? location ?loc)
  (test (not (eql ?loc ?place)))
  (not (goal-is-to ? action walk-to argument-1 ?place))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?place)))

(defrule drop-to-climb :forward :context mab
  (goal-is-to ? action on argument-1 ?obj)
  (thing ? name ?obj location ?place)
  (monkey ? location ?place holding ?hold)
  (test (not (eql ?hold 'blank)))
  (not (goal-is-to ? action hold argument-1 blank))
  -->
  (assert (goal-is-to ? action hold argument-1 blank)))

(defrule climb-indirectly :forward :context mab
  (goal-is-to ? action on argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on)
  (monkey ? location ?place on-top-of ?top holding blank)
  (test (and (not (eql ?top ?on))
             (not (eql ?top ?obj))))
  (not (goal-is-to ? action on argument-1 ?on))
  -->
  (assert (goal-is-to ? action on argument-1 ?on)))

(defrule climb-directly :forward :context mab
  (goal-is-to ?goal action on argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on)
  (monkey ?monkey location ?place on-top-of ?on holding blank)
  -->
  ((format t "Monkey climbs onto the ~A.~%" ?obj))
  (assert (monkey ?monkey on-top-of ?obj))
  (erase ?goal))

(defrule already-on-object :forward :context mab
  (goal-is-to ?goal action on argument-1 ?obj)
  (monkey ? on-top-of ?obj)
  -->
  (erase ?goal))

;;; Eat-object rules...

(defrule hold-to-eat :forward :context mab
  (goal-is-to ? action eat argument-1 ?obj)
  (monkey ? holding ?hold)
  (test (not (eql ?hold ?obj)))
  (not (goal-is-to ? action hold argument-1 ?obj))
  -->
  (assert (goal-is-to ? action hold argument-1 ?obj)))

(defrule satisfy-hunger :forward :context mab
  (goal-is-to ?goal action eat argument-1 ?name)
  (monkey ?monkey holding ?name)
  (thing ?thing name ?name)
  -->
  ((format t "Monkey eats the ~A.~%" ?name))
  (assert (monkey ?monkey holding blank))
  (erase ?goal)
  (erase ?thing))

;;; startup rule...

(defun startup ()
  (make-instance 'monkey :location 't5-7 :on-top-of 'green-couch
                 :holding 'blank)
  (make-instance 'thing :name 'green-couch :location 't5-7 :weight 'heavy
                 :on-top-of 'floor)
  (make-instance 'thing :name 'red-couch :location 't2-2 
                 :on-top-of 'floor :weight 'heavy)
  (make-instance 'thing :name 'big-pillow :location 't2-2 
                 :weight 'light :on-top-of 'red-couch)
  (make-instance 'thing :name 'red-chest :location 't2-2 
                 :weight 'light :on-top-of 'big-pillow)
  (make-instance 'chest :name 'red-chest :contents 'ladder :unlocked-by 'red-key)
  (make-instance 'thing :name 'blue-chest :location 't7-7 
                 :weight 'light :on-top-of 'ceiling)
  (make-instance 'thing :name 'grapes :location 't7-8 
                 :weight 'light :on-top-of 'ceiling)
  (make-instance 'chest :name 'blue-chest :contents 'bananas :unlocked-by 'blue-key)
  (make-instance 'thing :name 'blue-couch :location 't8-8 
                 :on-top-of 'floor :weight 'heavy)
  (make-instance 'thing :name 'green-chest :location 't8-8 
                 :weight 'light :on-top-of 'ceiling)
  (make-instance 'chest :name 'green-chest :contents 'blue-key :unlocked-by 'red-key)
  (make-instance 'thing :name 'red-key 
                 :on-top-of 'floor :weight 'light :location 't1-3)
  (make-instance 'goal-is-to :action 'eat :argument-1 'bananas))

(defun run-mab (&optional (ntimes 1))
  (flet ((repeat-mab ()
           (dotimes (i ntimes)
             (format t "Starting run.~%")
             (reset)
             (startup)
             (infer :contexts '(mab)))))
    (time (repeat-mab))))

(defun profile-mab (&optional (ntimes 10))
  (hcl:set-up-profiler :packages '("KW"))
  (hcl:profile (run-mab ntimes)))
