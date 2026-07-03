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

;; Description: An implementation of MYCIN as illustrated in PAIP, pg. 553. The example
;; is used to illustrate (and test) Lisa's support for uncertain reasoning. I didn't do
;; a faithful port of the PAIP version; in particular, there's no interaction with the
;; operator right now. However, all of the original rules are present and the two scenarios
;; on pgs. 555 and 556 are represented (by the functions CULTURE-1 and CULTURE-2).
;;
;; This example has been extended to exercise Lisa's pluggable belief-system protocol
;; (see src/belief-systems/). In addition to MYCIN-style certainty factors, the same
;; rulebase runs unchanged under a simplified Dempster-Shafer belief system, where each
;; hypothesis carries a [Bel, Pl] interval and the width (Pl - Bel) captures remaining
;; ignorance explicitly. Rule beliefs declared via (:belief ...) are interpreted by the
;; active belief system, and the CONCLUSION rule reports the combined belief factor for
;; each surviving organism-identity hypothesis via BELIEF:BELIEF-FACTOR.
;;
;; The rulebase has also been expanded beyond the original PAIP set to support
;; multi-hypothesis differentials that meaningfully exercise Dempster-Shafer combination:
;; hospital-acquired infections, immunocompromised hosts, respiratory sites, tropical
;; travel history, and low-WBC scenarios. The additional demonstration functions
;; CULTURE-1A and CULTURE-3 drive those cases and produce competing hypotheses whose
;; beliefs must be pooled by the active belief system.

(in-package :lisa-user)

(clear)

(setf lisa::*allow-duplicate-facts* nil)

(defclass param-mixin ()
  ((value :initarg :value
          :initform nil
          :reader value)
   (entity :initarg :entity
           :initform nil
           :reader entity)))

(defclass culture () ())

(defclass culture-site (param-mixin) ())

(defclass culture-age (param-mixin) ())

(defclass patient ()
  ((name :initarg :name
         :initform nil
         :reader name)
   (sex :initarg :sex
        :initform nil
        :reader sex)
   (age :initarg :age
        :initform nil
        :reader age)))

(defclass burn (param-mixin) ())

(defclass compromised-host (param-mixin) ())

(defclass organism () ())

(defclass gram (param-mixin) ())

(defclass morphology (param-mixin) ())

(defclass aerobicity (param-mixin) ())

(defclass growth-conformation (param-mixin) ())

(defclass hospital-acquired (param-mixin) ())

(defclass recent-travel (param-mixin) ())

(defclass white-blood-count (param-mixin) ())

(defclass infection-site (param-mixin) ())

(defclass organism-identity (param-mixin) ())

(defrule gram-neg-rod-in-burn-patient-suggests-pseudomonas (:belief 0.4)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (burn (value serious))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule gram-pos-cocci-in-clumps-suggests-staphylococcus (:belief 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value clumps))
  =>
  (assert (organism-identity (value staphylococcus) (entity ?organism))))

(defrule anaerobic-gram-neg-rod-in-blood-suggests-bacteroides (:belief 0.9)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (aerobicity (value anaerobic))
  =>
  (assert (organism-identity (value bacteroides) (entity ?organism))))

(defrule gram-neg-rod-in-compromised-host-suggests-pseudomonas (:belief 0.6)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (compromised-host (value t))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule aerobic-gram-neg-rod-suggests-enterobacteriaceae (:belief 0.8)
  (gram (value neg) (organism ?organism))
  (morphology (value rod))
  (aerobicity (value aerobic))
  =>
  (assert (organism-identity (value enterobacteriaceae) (entity ?organism))))

(defrule gram-pos-cocci-in-chains-suggests-streptococcus (:belief 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value chains))
  =>
  (assert (organism-identity (value streptococcus) (entity ?organism))))

;;; --- New rules: expanded rulebase for multi-hypothesis differentials ---

(defrule hospital-acquired-gram-pos-cocci-in-clumps-suggests-staph-aureus (:belief 0.8)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value clumps))
  (hospital-acquired (value t))
  =>
  (assert (organism-identity (value staphylococcus-aureus) (entity ?organism))))

(defrule hospital-acquired-gram-neg-rod-in-compromised-host-suggests-klebsiella (:belief 0.6)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (hospital-acquired (value t))
  (compromised-host (value t))
  =>
  (assert (organism-identity (value klebsiella) (entity ?organism))))

(defrule hospital-acquired-aerobic-gram-neg-rod-suggests-pseudomonas (:belief 0.7)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (aerobicity (value aerobic))
  (hospital-acquired (value t))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule aerobic-gram-neg-rod-in-compromised-host-suggests-klebsiella (:belief 0.5)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (aerobicity (value aerobic))
  (compromised-host (value t))
  =>
  (assert (organism-identity (value klebsiella) (entity ?organism))))

(defrule respiratory-gram-pos-cocci-in-chains-suggests-strep-pneumoniae (:belief 0.75)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value chains))
  (infection-site (value respiratory))
  =>
  (assert (organism-identity (value streptococcus-pneumoniae) (entity ?organism))))

(defrule gram-neg-rod-with-tropical-travel-suggests-salmonella (:belief 0.65)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (recent-travel (value tropical))
  =>
  (assert (organism-identity (value salmonella) (entity ?organism))))

(defrule gram-pos-cocci-in-chains-in-blood-compromised-suggests-enterococcus (:belief 0.7)
  (culture-site (value blood))
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value chains))
  (compromised-host (value t))
  =>
  (assert (organism-identity (value enterococcus) (entity ?organism))))

(defrule gram-neg-rod-in-blood-with-low-wbc-suggests-salmonella (:belief 0.55)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (culture-site (value blood))
  (white-blood-count (value low))
  =>
  (assert (organism-identity (value salmonella) (entity ?organism))))

(defrule anaerobic-gram-neg-rod-in-abdomen-suggests-bacteroides (:belief 0.8)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (aerobicity (value anaerobic))
  (infection-site (value abdominal))
  =>
  (assert (organism-identity (value bacteroides) (entity ?organism))))

;;; --- Ruling-out (disconfirming) rules ---
;;;
;;; Unlike every rule above, these inject *negative* evidence: a contradictory
;;; Gram stain or oxygen requirement argues AGAINST an organism hypothesis.
;;; They key off a live organism-identity hypothesis and re-assert it with a
;;; negative rule belief, which the active belief system folds in as
;;; disconfirming evidence.
;;;
;;; This is what makes Dempster-Shafer earn its keep: under DS a negative
;;; :belief becomes mass on not-H, so when it meets confirmatory evidence the
;;; combination produces real conflict (K > 0), lowering Bel AND pulling
;;; plausibility below 1.0 -- an ambiguous stain becomes visible as a widened,
;;; lowered interval. Under certainty factors it simply combines as a negative
;;; CF and collapses to a single number. With a purely confirmatory rulebase
;;; the two algebras are numerically identical; these rules are what pull them
;;; apart.

(defrule gram-pos-stain-argues-against-gram-neg-organism (:belief -0.7)
  (gram (value pos) (entity ?organism))
  (organism-identity (value ?value) (entity ?organism))
  (test (member ?value '(pseudomonas enterobacteriaceae klebsiella salmonella bacteroides)))
  =>
  (assert (organism-identity (value ?value) (entity ?organism))))

(defrule gram-neg-stain-argues-against-gram-pos-organism (:belief -0.7)
  (gram (value neg) (entity ?organism))
  (organism-identity (value ?value) (entity ?organism))
  (test (member ?value '(staphylococcus staphylococcus-aureus streptococcus
                         streptococcus-pneumoniae enterococcus)))
  =>
  (assert (organism-identity (value ?value) (entity ?organism))))

(defrule aerobic-growth-argues-against-anaerobe (:belief -0.8)
  (aerobicity (value aerobic) (entity ?organism))
  (organism-identity (value ?value) (entity ?organism))
  (test (member ?value '(bacteroides)))
  =>
  (assert (organism-identity (value ?value) (entity ?organism))))

;;; --- Conclusion rule ---

(defrule conclusion (:salience -10)
  (?identity (organism-identity (value ?value)))
  =>
  (format t "Identity: ~A (~,3F)~%" ?value (belief:belief-factor ?identity)))

(defun culture-1 (&key (runp t))
  "First PAIP scenario (pg. 555): aerobic gram-neg rod cultured from the blood of a
   seriously burned, immunocompromised patient. Evidence is asserted without explicit
   belief values, so each fact enters the working memory with the active belief system's
   default (full belief / no ignorance). Multiple rules fire on overlapping evidence,
   producing competing pseudomonas and enterobacteriaceae hypotheses that the belief
   system must combine."
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Sylvia Fischer"
                                 :sex 'female
                                 :age 27)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (burn (value serious) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (culture-age (value 3)))
    (assert (gram (value neg) (entity ?organism)))
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value aerobic) (entity ?organism)))
    (when runp
      (run))))

(defun culture-1a (&key (runp t))
  "Hospital-acquired gram-neg infection in immunocompromised patient.
   Should produce competing hypotheses: pseudomonas, klebsiella, enterobacteriaceae."
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Robert Chen"
                                 :sex 'male
                                 :age 62)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (hospital-acquired (value t) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (gram (value neg) (entity ?organism)))
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value aerobic) (entity ?organism)))
    (when runp
      (run))))

(defun culture-3 (&key (runp t))
  "Gram-pos cocci in chains from respiratory site in compromised host.
   Should produce competing hypotheses: streptococcus, streptococcus-pneumoniae, enterococcus."
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Maria Gonzales"
                                 :sex 'female
                                 :age 45)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (infection-site (value respiratory) (entity ?patient)))
    (assert (gram (value pos) (entity ?organism)))
    (assert (morphology (value coccus) (entity ?organism)))
    (assert (growth-conformation (value chains) (entity ?organism)))
    (when runp
      (run))))

(defun culture-2 (&key (runp t))
  "Second PAIP scenario (pg. 556): same burned, immunocompromised patient, but the Gram
   stain is ambiguous. Two conflicting GRAM facts are asserted with explicit belief values
   (0.8 for neg, 0.2 for pos), exercising the belief-system protocol on the fact side as
   well as the rule side. With an anaerobic gram-neg rod in the blood, the bacteroides
   rule dominates, but the streptococcus/staphylococcus rules also participate on the
   gram-pos branch — a good workout for both the certainty-factor and Dempster-Shafer
   combinators."
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Sylvia Fischer"
                                 :sex 'female
                                 :age 27)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (burn (value serious) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (culture-age (value 3)))
    (assert (gram (value neg) (entity ?organism)) :belief 0.8)
    (assert (gram (value pos) (entity ?organism)) :belief 0.2)
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value anaerobic) (entity ?organism)))
    (when runp
      (run))))
