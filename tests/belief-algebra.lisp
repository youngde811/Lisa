;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.
;; MIT License. Copyright (c) 2000 David Young.

;; Description: Unit tests for the pluggable belief algebras themselves —
;; certainty factors (Shortliffe-Buchanan) and simplified Dempster-Shafer.
;; These exercise combine/weaken/conjoin/normalize directly, independent of
;; the rulebase, including the clamp and total-conflict edge cases that the
;; 4.1.0 review surfaced.

(in-package "LISA-TEST")

;;; ------------------------------------------------------------------
;;; Certainty factors
;;; ------------------------------------------------------------------

(deftest cf-combine ()
  (let ((cf belief:*cf-system*))
    ;; both positive: a + b - a*b
    (is (approx= (belief:combine-beliefs cf 0.4 0.6) 0.76))
    ;; both negative: a + b + a*b
    (is (approx= (belief:combine-beliefs cf -0.4 -0.6) -0.76))
    ;; mixed sign: (a + b) / (1 - min(|a|,|b|))
    (is (approx= (belief:combine-beliefs cf 0.8 -0.5) 0.6))))

(deftest cf-weaken-and-conjoin ()
  (let ((cf belief:*cf-system*))
    (is (approx= (belief:weaken-belief cf 0.8 0.5) 0.4))       ; multiply
    (is (approx= (belief:conjoin-beliefs cf '(0.8 0.5 0.9)) 0.5)) ; weakest link
    (is (approx= (belief:normalize-belief cf 0.7) 0.7))))

;;; ------------------------------------------------------------------
;;; Dempster-Shafer: representation and normalization
;;; ------------------------------------------------------------------

(deftest ds-normalize ()
  (let ((ds belief:*ds-system*))
    ;; a bare number becomes a support interval [x, 1]
    (let ((r (belief:normalize-belief ds 0.7)))
      (is (approx= (belief:ds-belief-bel r) 0.7))
      (is (approx= (belief:ds-belief-pl r) 1.0)))
    ;; a negative number becomes disconfirming mass: [0, 1+x]
    (let ((r (belief:normalize-belief ds -0.3)))
      (is (approx= (belief:ds-belief-bel r) 0.0))
      (is (approx= (belief:ds-belief-pl r) 0.7)))))

;;; ------------------------------------------------------------------
;;; Dempster-Shafer: Dempster's rule of combination
;;; ------------------------------------------------------------------

(deftest ds-combine-no-conflict-is-noisy-or ()
  ;; With no disconfirming mass, K = 0 and the rule reduces to a+b-ab, pl = 1.
  (let* ((ds belief:*ds-system*)
         (r (belief:combine-beliefs ds
                                    (belief:make-ds-belief 0.4 1.0)
                                    (belief:make-ds-belief 0.6 1.0))))
    (is (approx= (belief:ds-belief-bel r) 0.76))
    (is (approx= (belief:ds-belief-pl r) 1.0))))

(deftest ds-combine-with-conflict-drops-plausibility ()
  ;; A "for" hypothesis (m(H)=0.76) meets an "against" one (m(not-H)=0.14).
  ;; Conflict K > 0 must renormalize: bel falls below 0.76 and pl below 1.0,
  ;; and the result must remain a valid interval.
  (let* ((ds belief:*ds-system*)
         (r (belief:combine-beliefs ds
                                    (belief:make-ds-belief 0.76 1.0)
                                    (belief:make-ds-belief 0.0 0.86))))
    (is (< (belief:ds-belief-pl r) 1.0) "conflict should pull pl below 1.0")
    (is (< (belief:ds-belief-bel r) 0.76) "conflict should reduce bel")
    (is (> (belief:ds-belief-bel r) 0.0))
    (is (belief:valid-belief-p ds r) "combined interval must be valid")))

(deftest ds-weaken-positive-and-negative ()
  (let ((ds belief:*ds-system*))
    ;; positive rule factor -> support for H (mass on H)
    (let ((r (belief:weaken-belief ds (belief:make-ds-belief 0.8 1.0) 0.5)))
      (is (approx= (belief:ds-belief-bel r) 0.4))
      (is (approx= (belief:ds-belief-pl r) 1.0)))
    ;; negative rule factor -> support against H (mass on not-H): pl = 1 - m
    (let ((r (belief:weaken-belief ds (belief:make-ds-belief 0.5 1.0) -0.8)))
      (is (approx= (belief:ds-belief-bel r) 0.0))
      (is (approx= (belief:ds-belief-pl r) 0.6)))))

(deftest ds-conjoin-is-min-min ()
  (let* ((ds belief:*ds-system*)
         (r (belief:conjoin-beliefs ds (list (belief:make-ds-belief 0.8 1.0)
                                             (belief:make-ds-belief 0.5 0.9)))))
    (is (approx= (belief:ds-belief-bel r) 0.5))
    (is (approx= (belief:ds-belief-pl r) 0.9))))

;;; ------------------------------------------------------------------
;;; Dempster-Shafer: defensive edge cases (from the 4.1.0 review)
;;; ------------------------------------------------------------------

(deftest ds-weaken-clamps-out-of-range-factor ()
  (let ((ds belief:*ds-system*))
    ;; |factor| > 1 must not produce an invalid interval
    (let ((r (belief:weaken-belief ds (belief:make-ds-belief 0.9 1.0) 1.5)))
      (is (approx= (belief:ds-belief-bel r) 1.0))
      (is (approx= (belief:ds-belief-pl r) 1.0))
      (is (belief:valid-belief-p ds r)))
    (let ((r (belief:weaken-belief ds (belief:make-ds-belief 0.9 1.0) -1.5)))
      (is (approx= (belief:ds-belief-bel r) 0.0))
      (is (approx= (belief:ds-belief-pl r) 0.0))
      (is (belief:valid-belief-p ds r)))))

(deftest ds-combine-total-conflict-yields-full-ignorance ()
  ;; K = 1 (irreconcilable) must return [0, 1] rather than divide by zero.
  (let* ((ds belief:*ds-system*)
         (r (belief:combine-beliefs ds
                                    (belief:make-ds-belief 1.0 1.0)
                                    (belief:make-ds-belief 0.0 0.0))))
    (is (approx= (belief:ds-belief-bel r) 0.0))
    (is (approx= (belief:ds-belief-pl r) 1.0))
    (is (belief:valid-belief-p ds r))))

(deftest ds-combine-clamps-malformed-input ()
  ;; A malformed input interval (bel > pl) must still yield a valid result.
  (let* ((ds belief:*ds-system*)
         (r (belief:combine-beliefs ds
                                    (belief:make-ds-belief 0.6 1.0)
                                    (belief:make-ds-belief 0.9 0.3))))
    (is (<= (belief:ds-belief-bel r) (belief:ds-belief-pl r)))
    (is (belief:valid-belief-p ds r))))