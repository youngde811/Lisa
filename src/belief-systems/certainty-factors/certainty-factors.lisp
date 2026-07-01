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

;; Description: Certainty Factors (Shortliffe-Buchanan) implementation of the
;; pluggable belief system protocol.

(in-package :belief)

;;; ============================================================
;;; Certainty Factor System
;;; ============================================================

(defclass certainty-factor-system (belief-system)
  ()
  (:default-initargs :name "Certainty Factors (Shortliffe-Buchanan)"))

(defmethod valid-belief-p ((system certainty-factor-system) value)
  (and (realp value) (<= -1.0 value 1.0)))

(defmethod normalize-belief ((system certainty-factor-system) value)
  (coerce value 'single-float))

(defmethod combine-beliefs ((system certainty-factor-system) a b)
  "Classic CF combination formula from MYCIN."
  (cond ((and (plusp a) (plusp b))
         (+ a b (* -1.0 a b)))
        ((and (minusp a) (minusp b))
         (+ a b (* a b)))
        (t
         (/ (+ a b) (- 1.0 (min (abs a) (abs b)))))))

(defmethod weaken-belief ((system certainty-factor-system) belief factor)
  (* belief factor))

(defmethod conjoin-beliefs ((system certainty-factor-system) beliefs)
  "AND semantics: weakest link."
  (apply #'min beliefs))

(defmethod belief->english ((system certainty-factor-system) cf)
  (cond ((null cf) "no evidence")
        ((= cf 1.0) "certain")
        ((> cf 0.8) "strongly suggestive")
        ((> cf 0.5) "suggestive")
        ((> cf 0.0) "weakly suggestive")
        ((= cf 0.0) "unknown")
        ((< cf 0.0)
         (format nil "~A against" (belief->english system (- cf))))))

(defmethod belief->json ((system certainty-factor-system) cf)
  cf)

(defvar *cf-system* (make-instance 'certainty-factor-system)
  "Singleton certainty factor system instance.")

;;; Default: Lisa uses certainty factors unless explicitly switched.
(unless *belief-system*
  (setf *belief-system* *cf-system*))