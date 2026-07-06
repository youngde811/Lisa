;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.
;; MIT License. Copyright (c) 2000 David Young.

;; Description: A tiny, dependency-free test harness for Lisa. Provides test
;; registration (DEFTEST), float-tolerant assertions, belief-system-aware
;; checks against a scenario's conclusions, and a runner (RUN-ALL) that prints
;; a pass/fail report. No external test framework is used on purpose: golden-
;; master numeric assertions need very little machinery, and keeping the test
;; system dependency-free mirrors Lisa's own minimalism.

(defpackage "LISA-TEST"
  (:use "COMMON-LISP")
  (:export "RUN-ALL" "DEFTEST"))

(in-package "LISA-TEST")

;;; ------------------------------------------------------------------
;;; Registry and counters
;;; ------------------------------------------------------------------

(defvar *tests* '()
  "Ordered list of (name . thunk); thunks run under RUN-ALL.")
(defvar *pass* 0)
(defvar *fail* 0)
(defvar *failures* '())
(defvar *current* nil "Name of the test currently executing.")

(defparameter *tolerance* 1.0e-4
  "Absolute tolerance for float comparisons of belief values.")

(defun approx= (a b &optional (tol *tolerance*))
  (<= (abs (- a b)) tol))

(defun register-test (name thunk)
  ;; Redefining a test replaces it in place (append keeps definition order).
  (setf *tests* (append (remove name *tests* :key #'car) (list (cons name thunk)))))

(defmacro deftest (name arglist &body body)
  "Register a test named NAME. ARGLIST is accepted for readability and ignored."
  (declare (ignore arglist))
  `(register-test ',name (lambda () ,@body)))

;;; ------------------------------------------------------------------
;;; Assertions
;;; ------------------------------------------------------------------

(defun record-pass () (incf *pass*))

(defun record-fail (fmt &rest args)
  (incf *fail*)
  (push (cons *current* (apply #'format nil fmt args)) *failures*))

(defmacro is (form &optional description)
  "Assert FORM is non-nil. On failure, report DESCRIPTION or the form itself."
  (let ((desc (or description `(quote ,form))))
    `(if ,form (record-pass) (record-fail "~A" ,desc))))

(defun check-cf (conclusions name expected)
  "Assert organism NAME has a certainty factor within tolerance of EXPECTED."
  (let ((b (belief-of conclusions name)))
    (cond ((null b) (record-fail "~A: expected CF ~,4F but organism absent" name expected))
          ((not (realp b)) (record-fail "~A: expected a CF number, got ~S" name b))
          ((approx= b expected) (record-pass))
          (t (record-fail "~A: CF expected ~,4F, got ~,4F" name expected b)))))

(defun check-ds (conclusions name expected-bel expected-pl)
  "Assert organism NAME has a DS interval within tolerance of [bel, pl]."
  (let ((b (belief-of conclusions name)))
    (cond
      ((null b)
       (record-fail "~A: expected DS [~,4F, ~,4F] but organism absent"
                    name expected-bel expected-pl))
      ((not (belief:ds-belief-p b))
       (record-fail "~A: expected a ds-belief, got ~S" name b))
      (t
       (if (approx= (belief:ds-belief-bel b) expected-bel) (record-pass)
           (record-fail "~A: DS bel expected ~,4F, got ~,4F"
                        name expected-bel (belief:ds-belief-bel b)))
       (if (approx= (belief:ds-belief-pl b) expected-pl) (record-pass)
           (record-fail "~A: DS pl expected ~,4F, got ~,4F"
                        name expected-pl (belief:ds-belief-pl b)))))))

(defun check-absent (conclusions name)
  "Assert organism NAME was not concluded."
  (if (belief-of conclusions name)
      (record-fail "~A: expected absent but it was concluded" name)
      (record-pass)))

;;; ------------------------------------------------------------------
;;; Scenario driving (reuses the same fact-query path as the bridge)
;;; ------------------------------------------------------------------

(defvar *rulebase-loaded* nil)

(defun ensure-rulebase ()
  "Load the MYCIN example rulebase once (defines classes, rules, culture-* fns)."
  (unless *rulebase-loaded*
    (let ((*standard-output* (make-broadcast-stream)))
      (load (asdf:system-relative-pathname "lisa" "examples/mycin.lisp")))
    (setf *rulebase-loaded* t)))

(defun collect-conclusions ()
  "Return an alist (organism-name-string . belief) for every organism-identity
   fact in working memory — the same query the bridge's /conclusions uses."
  (let ((acc '()))
    (dolist (fact (lisa:get-fact-list (lisa:inference-engine)))
      (when (eq (lisa:fact-name fact) 'lisa-user::organism-identity)
        (push (cons (string-downcase
                     (symbol-name (lisa:get-slot-value fact 'lisa-user::value)))
                    (belief:belief-factor fact))
              acc)))
    acc))

(defun run-scenario (fn-designator system)
  "Select belief SYSTEM (:certainty-factors | :dempster-shafer), run scenario
   FN-DESIGNATOR (a culture-* function that resets and runs), and return its
   conclusions alist."
  (belief:use-system system)
  ;; The MYCIN rulebase's own conclusion rule prints each identity as it fires;
  ;; discard that so the test report stays clean. We read conclusions from
  ;; working memory ourselves.
  (let ((*standard-output* (make-broadcast-stream)))
    (funcall fn-designator))
  (collect-conclusions))

(defun belief-of (conclusions name)
  "Look up the belief for organism NAME (case-insensitive) in CONCLUSIONS."
  (cdr (assoc (string-downcase name) conclusions :test #'string=)))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun run-all ()
  "Run every registered test, print a report, and return T iff all passed."
  (ensure-rulebase)
  (setf *pass* 0 *fail* 0 *failures* '())
  (dolist (entry *tests*)
    (let ((*current* (car entry)))
      (handler-case (funcall (cdr entry))
        (error (e) (record-fail "signalled ~A: ~A" (type-of e) e)))))
  (format t "~&~%========================================~%")
  (format t "LISA TEST SUITE: ~D passed, ~D failed  (~D tests)~%"
          *pass* *fail* (length *tests*))
  (format t "========================================~%")
  (when *failures*
    (format t "~%Failures:~%")
    (dolist (f (reverse *failures*))
      (format t "  [~(~A~)] ~A~%" (car f) (cdr f))))
  (finish-output)
  (zerop *fail*))