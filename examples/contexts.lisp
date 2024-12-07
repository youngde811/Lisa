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

(in-package :lisa-user)

(defcontext :hobbits)
(defcontext :wizards)
(defcontext :elves)
(defcontext :dwarves)

(deftemplate frodo ()
  (slot name))

(deftemplate gandalf ()
  (slot name))

(deftemplate legolas ()
  (slot name))

(deftemplate gimli ()
  (slot name))

(defrule frodo (:context :hobbits)
  (frodo)
  =>
  (format t "frodo fired; focusing on :wizards.~%")
  (assert (gandalf (name gandalf)))
  (focus :wizards))

(defrule gandalf (:context :wizards)
  (gandalf (name gandalf))
  =>
  (format t "gandalf fired; gimli should fire now.~%")
  (assert (legolas))
  (assert (gimli)))

(defrule legolas (:context :elves)
  (legolas)
  =>
  (format t "legolas firing; hopefully this was a manual focus.~%"))

(defrule gimli (:context :dwarves :auto-focus t :salience 100)
  (gimli)
  =>
  (format t "gimli (an auto-focus rule) fired.~%")
  (refocus))

(defrule should-not-fire (:context :dwarves)
  (gimli)
  =>
  (error "This rule should not have fired!"))

(defrule start (:salience 100)
  =>
  (format t "starting...~%")
  (focus :hobbits))

(defrule finish ()
  (?gimli (gimli))
  =>
  (retract ?gimli)
  (format t "finished.~%"))

(reset)
(assert (frodo))
(run)
