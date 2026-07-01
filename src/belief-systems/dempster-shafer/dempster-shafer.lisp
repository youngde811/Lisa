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

;; Description: Simplified Dempster-Shafer belief system implementation.
;; Represents belief as [Bel, Pl] intervals where the width (Pl - Bel)
;; captures remaining ignorance explicitly.

(in-package :belief)

;;; ============================================================
;;; Belief Representation
;;; ============================================================

(defstruct (ds-belief
            (:constructor make-ds-belief (bel pl))
            (:print-function print-ds-belief))
  "Simplified Dempster-Shafer belief as [Bel, Pl] interval.

   BEL (belief): lower bound — evidence that directly supports the hypothesis
   PL (plausibility): upper bound — 1 minus evidence that rules out the hypothesis

   The interval width (PL - BEL) represents remaining ignorance."
  (bel 0.0 :type single-float)
  (pl  1.0 :type single-float))

(defun print-ds-belief (ds stream depth)
  (declare (ignore depth))
  (format stream "[~,2F, ~,2F]" (ds-belief-bel ds) (ds-belief-pl ds)))

(defun ds-ignorance (ds)
  "Return the ignorance (uncertainty width) of a DS belief."
  (- (ds-belief-pl ds) (ds-belief-bel ds)))

(defun ds-midpoint (ds)
  "Return the midpoint of the belief interval."
  (/ (+ (ds-belief-bel ds) (ds-belief-pl ds)) 2.0))

;;; ============================================================
;;; Dempster-Shafer System Class
;;; ============================================================

(defclass dempster-shafer-system (belief-system)
  ((default-pl :initarg :default-pl
               :initform 1.0
               :reader default-plausibility
               :documentation "Default plausibility for new evidence."))
  (:default-initargs :name "Dempster-Shafer (simplified)"))

(defmethod valid-belief-p ((system dempster-shafer-system) value)
  (or (null value)
      (and (ds-belief-p value)
           (<= 0.0 (ds-belief-bel value))
           (<= (ds-belief-bel value) (ds-belief-pl value))
           (<= (ds-belief-pl value) 1.0))))

(defmethod normalize-belief ((system dempster-shafer-system) value)
  "Convert input to DS belief.
   - Number in [0,1]: treated as belief with full plausibility
   - Number in [-1,0): treated as evidence against (low plausibility)
   - DS-belief: used as-is"
  (etypecase value
    (null (make-ds-belief 0.0 1.0))
    (ds-belief value)
    (number
     (cond ((>= value 0.0)
            (make-ds-belief (coerce value 'single-float) 1.0))
           (t
            (make-ds-belief 0.0 (coerce (+ 1.0 value) 'single-float)))))))

;;; ============================================================
;;; Core Algebra
;;; ============================================================

(defmethod combine-beliefs ((system dempster-shafer-system) a b)
  "Combine two DS beliefs for the same hypothesis.

   Uses asymptotic combination for the belief component (lower bound)
   and minimum for plausibility (conservative upper bound).

   Note: Full DS would require mass functions over the power set.
   This approximation works well for single-hypothesis reasoning."
  (let ((bel-a (ds-belief-bel a))
        (bel-b (ds-belief-bel b))
        (pl-a (ds-belief-pl a))
        (pl-b (ds-belief-pl b)))
    (let ((combined-bel (+ bel-a bel-b (* -1.0 bel-a bel-b)))
          (combined-pl (min pl-a pl-b)))
      (make-ds-belief (min combined-bel combined-pl) combined-pl))))

(defmethod weaken-belief ((system dempster-shafer-system) belief rule-factor)
  "Apply rule weight to belief.

   Scales belief proportionally; plausibility moves toward 1.0
   (weaker evidence = more uncertainty)."
  (let* ((bel (ds-belief-bel belief))
         (pl (ds-belief-pl belief))
         (f (coerce rule-factor 'single-float))
         (new-bel (* bel f))
         (new-pl (+ (* pl f) (* 1.0 (- 1.0 f)))))
    (make-ds-belief new-bel (max new-bel new-pl))))

(defmethod conjoin-beliefs ((system dempster-shafer-system) beliefs)
  "Combine beliefs from AND-ed premises.

   AND semantics:
   - Belief is minimum (weakest link)
   - Plausibility is minimum (if any premise is doubtful, conclusion is)"
  (make-ds-belief
   (apply #'min (mapcar #'ds-belief-bel beliefs))
   (apply #'min (mapcar #'ds-belief-pl beliefs))))

;;; ============================================================
;;; Display and Serialization
;;; ============================================================

(defmethod belief->number ((system dempster-shafer-system) belief)
  "Single number for sorting: use belief (lower bound)."
  (if (ds-belief-p belief)
      (ds-belief-bel belief)
      0.0))

(defmethod belief->english ((system dempster-shafer-system) belief)
  (cond
    ((null belief)
     "no evidence")
    ((not (ds-belief-p belief))
     (format nil "~A" belief))
    (t
     (let* ((bel (ds-belief-bel belief))
            (pl (ds-belief-pl belief))
            (ignorance (- pl bel)))
       (cond
         ((and (> bel 0.7) (< ignorance 0.2))
          (format nil "strong evidence (~,0F%)" (* 100 bel)))
         ((and (> bel 0.5) (>= ignorance 0.3))
          (format nil "suggestive (~,0F%) but uncertain" (* 100 bel)))
         ((> bel 0.3)
          (format nil "moderate evidence (~,0F%)" (* 100 bel)))
         ((< pl 0.3)
          (format nil "largely ruled out (plausibility ~,0F%)" (* 100 pl)))
         ((and (< bel 0.2) (> pl 0.8))
          "insufficient evidence")
         (t
          (format nil "belief ~,0F-~,0F%%" (* 100 bel) (* 100 pl))))))))

(defmethod belief->json ((system dempster-shafer-system) belief)
  "JSON representation: object with bel and pl fields."
  (cond
    ((null belief) nil)
    ((ds-belief-p belief)
     `(("bel" . ,(ds-belief-bel belief))
       ("pl" . ,(ds-belief-pl belief))
       ("ignorance" . ,(ds-ignorance belief))))
    (t belief)))

;;; ============================================================
;;; Singleton
;;; ============================================================

(defvar *ds-system* (make-instance 'dempster-shafer-system)
  "Singleton simplified Dempster-Shafer system instance.")