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

;; Description: Dempster-Shafer belief system implementation.
;; Represents belief as [Bel, Pl] intervals where the width (Pl - Bel)
;; captures remaining ignorance explicitly. Each interval is a basic
;; probability assignment over the dichotomous frame {H, not-H}
;; (m(H)=Bel, m(not-H)=1-Pl, m(Theta)=Pl-Bel), and COMBINE-BELIEFS applies
;; Dempster's rule of combination with explicit conflict (K) renormalization.
;; Restricting the frame to a single hypothesis and its negation (the Barnett
;; simplification) keeps combination O(1) and avoids power-set mass functions
;; while remaining faithful DS on that frame -- hence the "(simplified)" name.

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
  "Combine two DS beliefs for the same hypothesis via Dempster's rule of
   combination on the dichotomous frame {H, not-H}.

   Each [Bel, Pl] interval encodes a basic probability assignment over that
   frame:
     m(H)     = Bel        — mass committed to the hypothesis
     m(not-H) = 1 - Pl     — mass committed against it
     m(Theta) = Pl - Bel   — uncommitted mass (ignorance)

   Dempster's rule intersects the focal elements of the two BPAs, tallies the
   conflict mass K (the mass that lands on the empty set, H and not-H being
   disjoint), and renormalizes the survivors by 1 - K:

     K       = m1(H)*m2(not-H) + m1(not-H)*m2(H)
     m12(H)  = [m1(H)m2(H) + m1(H)m2(Theta) + m1(Theta)m2(H)] / (1 - K)
     m12(nH) = [m1(nH)m2(nH) + m1(nH)m2(Theta) + m1(Theta)m2(nH)] / (1 - K)

   This is genuine Dempster-Shafer, restricted to singleton hypotheses (the
   Barnett simplification) so it stays O(1) per combination and needs no
   power-set machinery. When there is no disconfirming evidence
   (m(not-H) = 0 on both sides) K collapses to 0 and the rule reduces to the
   familiar Bel = a + b - a*b, Pl = 1 form, so confirmatory-only scenarios are
   numerically unchanged from before."
  (let* ((h1 (ds-belief-bel a)) (nh1 (- 1.0 (ds-belief-pl a)))
         (th1 (- (ds-belief-pl a) (ds-belief-bel a)))
         (h2 (ds-belief-bel b)) (nh2 (- 1.0 (ds-belief-pl b)))
         (th2 (- (ds-belief-pl b) (ds-belief-bel b)))
         (k (+ (* h1 nh2) (* nh1 h2))))
    (if (>= k 1.0)
        ;; Total conflict: the sources are irreconcilable. Return full
        ;; ignorance rather than dividing by zero (a Dempster-vs-Yager choice;
        ;; full ignorance is the conservative, non-committal option).
        (make-ds-belief 0.0 1.0)
        (let* ((norm (- 1.0 k))
               (mh  (/ (+ (* h1 h2) (* h1 th2) (* th1 h2)) norm))
               (mnh (/ (+ (* nh1 nh2) (* nh1 th2) (* th1 nh2)) norm))
               ;; Clamp defensively: with well-formed inputs (bel <= pl in [0,1])
               ;; these already land in range, but a malformed input interval
               ;; would otherwise leak negative or >1 masses into the result.
               (bel (max 0.0 (min 1.0 mh)))
               (pl  (max 0.0 (min 1.0 (- 1.0 mnh)))))
          (make-ds-belief (min bel pl) pl)))))

(defmethod weaken-belief ((system dempster-shafer-system) belief rule-factor)
  "Scale the premise support by a rule's intrinsic belief, producing a simple
   support function for the conclusion.

   A positive RULE-FACTOR yields support FOR the hypothesis: mass RULE-FACTOR *
   premise-strength lands on H, the rest on Theta. A negative RULE-FACTOR
   yields support AGAINST it: the mass lands on not-H instead — this is how
   ruling-out rules inject disconfirming evidence that COMBINE-BELIEFS can then
   turn into genuine conflict. Plausibility falls out of the mass placement:
   1.0 for supporting evidence, 1 - mass for disconfirming evidence."
  (let* ((strength (ds-belief-bel belief))            ; support carried by premises
         (f (coerce rule-factor 'single-float))
         ;; Clamp to [0,1] so an out-of-range rule :belief (|factor| > 1) can't
         ;; produce an invalid interval (bel > pl or a negative bound).
         (mass (min 1.0 (max 0.0 (* strength (abs f))))))
    (if (minusp f)
        (make-ds-belief 0.0 (- 1.0 mass))                    ; against: m(not-H)=mass
        (make-ds-belief mass (ds-belief-pl belief)))))       ; for: m(H)=mass, carry premise Pl

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