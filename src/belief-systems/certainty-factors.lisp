
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

;; File: certainty-factors.lisp

;; Description: An implementation of Certainty Factors as found in Peter Norvig's PAIP.

(in-package :belief)

(defconstant +true+ 1.0)
(defconstant +false+ -1.0)
(defconstant +unknown+ 0.0)

(defun certainty-factor-p (number)
  (<= +false+ number +true+))

(deftype certainty-factor ()
  `(and (real)
        (satisfies certainty-factor-p)))

(defun true-p (cf)
  (check-type cf certainty-factor)
  (> cf +unknown+))

(defun false-p (cf)
  (check-type cf certainty-factor)
  (< cf +unknown+))

(defun unknown-p (cf)
  (check-type cf certainty-factor)
  (= cf +unknown+))

(defun cf-combine (a b)
  (check-type a certainty-factor)
  (check-type b certainty-factor)
  (cond ((and (plusp a)
              (plusp b))
         (+ a b (* -1 a b)))
        ((and (minusp a)
              (minusp b))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defun conjunct-cf (objects)
  "Combines the certainty factors of objects matched within a single rule."
  (let ((conjuncts
         (loop for obj in objects
               for cf = (belief-factor obj)
               if cf collect cf)))
    (if conjuncts
        (apply #'min conjuncts)
      nil)))

(defgeneric recalculate-cf (objects rule-cf old-cf)
  (:method (objects (rule-cf number) (old-cf number))
   (let* ((combined-cf (conjunct-cf objects))
          (new-cf (if combined-cf (* rule-cf combined-cf) rule-cf)))
     (cf-combine old-cf new-cf)))
  (:method (objects (rule-cf number) (old-cf t))
   (let* ((combined-cf (conjunct-cf objects))
          (new-cf (if combined-cf combined-cf rule-cf))
          (factor (if combined-cf rule-cf 1.0)))
     (* new-cf factor)))
  (:method (objects (rule-cf t) (old-cf t))
   (let* ((combined-cf (conjunct-cf objects)))
     (if combined-cf
         (* combined-cf 1.0)
       nil))))

(defun cf->english (cf)
  (cond ((= cf 1.0) "certain evidence")
        ((> cf 0.8) "strongly suggestive evidence")
        ((> cf 0.5) "suggestive evidence")
        ((> cf 0.0) "weakly suggestive evidence")
        ((= cf 0.0) "no evidence either way")
        ((< cf 0.0) (concatenate 'string (cf->english (- cf)) " against the conclusion"))))

;;; interface into the generic belief system.

(defmethod adjust-belief (objects (rule-belief number) &optional (old-belief nil))
  (recalculate-cf objects rule-belief old-belief))

(defmethod adjust-belief (objects (rule-belief t) &optional old-belief)
  (declare (ignore objects old-belief))
  nil)

(defmethod belief->english ((cf number))
  (cf->english cf))

