;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.
;; MIT License. Copyright (c) 2000 David Young.

;; Description: Per-rule coverage. Each of the 18 MYCIN rules is fired in
;; isolation on a minimal premise set that lets *only that rule* conclude the
;; target organism, and the resulting belief is asserted. Confirming rules
;; (1-15) must contribute exactly their :belief (CF) / [belief, 1.0] (DS); the
;; three disconfirming rules (16-18) must lower an existing hypothesis and pull
;; its plausibility below 1.0.

(in-package "LISA-TEST")

;;; Helper: assert the isolated confirming belief holds under BOTH algebras.
(defun check-rule (builder organism belief)
  "Run BUILDER under CF and DS; assert ORGANISM is concluded with BELIEF (CF)
   and [BELIEF, 1.0] (DS) — i.e. exactly this rule's contribution, alone."
  (check-cf (run-facts :certainty-factors builder) organism belief)
  (check-ds (run-facts :dempster-shafer builder) organism belief 1.0))

;;; ------------------------------------------------------------------
;;; Confirming rules (1-15) — fired in isolation
;;; ------------------------------------------------------------------

(deftest rule-gram-neg-rod-burn-pseudomonas ()      ; 0.4
  (check-rule (lambda (o p)
                (af "culture-site" "blood")
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "burn" "serious" p))
              "pseudomonas" 0.4))

(deftest rule-gram-pos-cocci-clumps-staphylococcus () ; 0.7
  (check-rule (lambda (o p) (declare (ignore p))
                (af "gram" "pos" o) (af "morphology" "coccus" o)
                (af "growth-conformation" "clumps" o))
              "staphylococcus" 0.7))

(deftest rule-anaerobic-gram-neg-rod-blood-bacteroides () ; 0.9
  (check-rule (lambda (o p) (declare (ignore p))
                (af "culture-site" "blood")
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "aerobicity" "anaerobic" o))
              "bacteroides" 0.9))

(deftest rule-gram-neg-rod-compromised-pseudomonas () ; 0.6
  (check-rule (lambda (o p)
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "compromised-host" "t" p))
              "pseudomonas" 0.6))

(deftest rule-aerobic-gram-neg-rod-enterobacteriaceae () ; 0.8
  (check-rule (lambda (o p) (declare (ignore p))
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "aerobicity" "aerobic" o))
              "enterobacteriaceae" 0.8))

(deftest rule-gram-pos-cocci-chains-streptococcus () ; 0.7
  (check-rule (lambda (o p) (declare (ignore p))
                (af "gram" "pos" o) (af "morphology" "coccus" o)
                (af "growth-conformation" "chains" o))
              "streptococcus" 0.7))

(deftest rule-hospital-gram-pos-cocci-clumps-staph-aureus () ; 0.8
  (check-rule (lambda (o p)
                (af "gram" "pos" o) (af "morphology" "coccus" o)
                (af "growth-conformation" "clumps" o)
                (af "hospital-acquired" "t" p))
              "staphylococcus-aureus" 0.8))

(deftest rule-hospital-gram-neg-rod-compromised-klebsiella () ; 0.6
  (check-rule (lambda (o p)
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "hospital-acquired" "t" p) (af "compromised-host" "t" p))
              "klebsiella" 0.6))

(deftest rule-hospital-aerobic-gram-neg-rod-pseudomonas () ; 0.7
  (check-rule (lambda (o p)
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "aerobicity" "aerobic" o) (af "hospital-acquired" "t" p))
              "pseudomonas" 0.7))

(deftest rule-aerobic-gram-neg-rod-compromised-klebsiella () ; 0.5
  (check-rule (lambda (o p)
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "aerobicity" "aerobic" o) (af "compromised-host" "t" p))
              "klebsiella" 0.5))

(deftest rule-respiratory-gram-pos-cocci-chains-strep-pneumoniae () ; 0.75
  (check-rule (lambda (o p)
                (af "gram" "pos" o) (af "morphology" "coccus" o)
                (af "growth-conformation" "chains" o)
                (af "infection-site" "respiratory" p))
              "streptococcus-pneumoniae" 0.75))

(deftest rule-gram-neg-rod-travel-salmonella () ; 0.65
  (check-rule (lambda (o p)
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "recent-travel" "tropical" p))
              "salmonella" 0.65))

(deftest rule-gram-pos-cocci-chains-blood-compromised-enterococcus () ; 0.7
  (check-rule (lambda (o p)
                (af "culture-site" "blood")
                (af "gram" "pos" o) (af "morphology" "coccus" o)
                (af "growth-conformation" "chains" o)
                (af "compromised-host" "t" p))
              "enterococcus" 0.7))

(deftest rule-gram-neg-rod-blood-low-wbc-salmonella () ; 0.55
  (check-rule (lambda (o p)
                (af "culture-site" "blood")
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "white-blood-count" "low" p))
              "salmonella" 0.55))

(deftest rule-anaerobic-gram-neg-rod-abdomen-bacteroides () ; 0.8
  (check-rule (lambda (o p)
                (af "gram" "neg" o) (af "morphology" "rod" o)
                (af "aerobicity" "anaerobic" o)
                (af "infection-site" "abdominal" p))
              "bacteroides" 0.8))

;;; ------------------------------------------------------------------
;;; Disconfirming rules (16-18) — must lower a live hypothesis (pl < 1.0)
;;; ------------------------------------------------------------------

(defun check-disconfirms (builder organism confirming-belief)
  "Run BUILDER under DS; assert ORGANISM is still present but its belief fell
   below CONFIRMING-BELIEF and its plausibility below 1.0 (a disconfirming rule
   fired). Also verify the CF belief dropped below the confirming value."
  (let ((ds (run-facts :dempster-shafer builder))
        (cf (run-facts :certainty-factors builder)))
    (let ((b (belief-of ds organism)))
      (cond ((null b) (record-fail "~A: expected present (disconfirmed), absent" organism))
            (t (is (< (belief:ds-belief-pl b) 1.0)
                   (format nil "~A plausibility should be < 1.0 after disconfirmation" organism))
               (is (< (belief:ds-belief-bel b) confirming-belief)
                   (format nil "~A belief should fall below ~,2F after disconfirmation"
                           organism confirming-belief)))))
    (is (< (belief-of cf organism) confirming-belief)
        (format nil "~A CF should fall below ~,2F after disconfirmation"
                organism confirming-belief))))

(deftest rule-gram-pos-argues-against-gram-neg-organism ()
  ;; A gram-positive reading disconfirms a gram-negative hypothesis (pseudomonas
  ;; established at 0.6 via the compromised-host rule).
  (check-disconfirms (lambda (o p)
                       (af "compromised-host" "t" p)
                       (af "gram" "neg" o) (af "morphology" "rod" o)
                       (af "gram" "pos" o))
                     "pseudomonas" 0.6))

(deftest rule-gram-neg-argues-against-gram-pos-organism ()
  ;; A gram-negative reading disconfirms a gram-positive hypothesis
  ;; (streptococcus established at 0.7 via the cocci-in-chains rule).
  (check-disconfirms (lambda (o p) (declare (ignore p))
                       (af "gram" "pos" o) (af "morphology" "coccus" o)
                       (af "growth-conformation" "chains" o)
                       (af "gram" "neg" o))
                     "streptococcus" 0.7))

(deftest rule-aerobic-argues-against-anaerobe ()
  ;; Aerobic growth disconfirms bacteroides (a strict anaerobe), established at
  ;; 0.9 via the anaerobic-blood rule; the contradictory aerobic fact fires the
  ;; ruling-out rule.
  (check-disconfirms (lambda (o p) (declare (ignore p))
                       (af "culture-site" "blood")
                       (af "gram" "neg" o) (af "morphology" "rod" o)
                       (af "aerobicity" "anaerobic" o)
                       (af "aerobicity" "aerobic" o))
                     "bacteroides" 0.9))