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

;;; Description: This tiny rulebase was written to verify a fix to Lisa's TEST
;;; conditional element, reported by Gaston Pepe.

(in-package :lisa-user)

(deftemplate hobbit ()
    (slot name))

(defrule check-frodo ()
  (hobbit (name ?name))
  (test (equal ?name "frodo"))
  =>
  (format t "Frodo found!~%")
  (assert (hobbit (name "bilbo"))))

(defrule check-bilbo ()
  (hobbit (name ?name))
  (test (or (equal ?name "bilbo")
            (equal ?name "pippin")))
  =>
  (format t "Frodo's bud ~A found!~%" ?name))

(defun hobbits ()
  (reset)
  (assert (hobbit (name "frodo")))
  (run)
  (facts)
  (reset)
  (facts)
  t)
