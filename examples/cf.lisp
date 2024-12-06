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

;; Description: Test code for Lisa's certainty factor support.

(in-package :lisa-user)

(defclass hobbit ()
  ((name :initarg :name
         :reader name)))

(defclass has-ring () ())

(defclass two-clowns () ())

(defrule frodo ()
  (hobbit (name frodo))
  =>
  (assert (has-ring) :cf 0.9))

(defrule bilbo (:cf 0.3)
  (hobbit (name bilbo))
  =>
  (assert (has-ring)))

(defrule combine (:cf 0.6)
  (?a (hobbit (name merry)))
  (?b (hobbit (name pippin)))
  =>
  (assert (two-clowns)))

(defrule combine-2 (:cf 0.9)
  (?a (hobbit (name sam)))
  (?b (hobbit (name bilbo)))
  =>
  (assert (two-clowns)))

(defun combine ()
  (assert (hobbit (name merry)) :cf 0.8)
  (assert (hobbit (name pippin)) :cf 0.2)
  (run))
