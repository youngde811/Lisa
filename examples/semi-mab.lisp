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

(in-package :lisa)

(use-default-engine)

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

(defrule hold-chest-to-put-on-floor ()
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (on-top-of (not floor)) (weight light))
  (monkey (holding (not ?chest)))
  (not (goal-is-to (action hold) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?chest)
                      (argument-2 empty))))

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

(defrule hold-to-eat ()
  (goal-is-to (action eat) (argument-1 ?obj))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1 ?obj)))
  =>
  (format t "firing hold-to-eat~%")
  (assert (goal-is-to (action hold) (argument-1 ?obj)
                      (argument-2 empty))))

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
