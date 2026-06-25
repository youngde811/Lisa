;;;; Simplified Dempster-Shafer Belief System Prototype
;;;;
;;;; This file contains a prototype implementation for Lisa's pluggable
;;;; belief system architecture with a simplified Dempster-Shafer algebra.
;;;;
;;;; Status: PROTOTYPE — do not load directly. For review and discussion.
;;;;
;;;; Design goals:
;;;;   1. Pluggable: swap belief algebras at runtime
;;;;   2. Backward compatible: existing CF rules still work
;;;;   3. Minimal storage change: struct instead of number
;;;;   4. Explicit ignorance: [Bel, Pl] intervals surface uncertainty

;;; ============================================================
;;; Part 1: Belief System Protocol
;;; ============================================================
;;; File: src/belief-systems/protocol.lisp

(in-package :belief)

(defclass belief-system ()
  ((name :initarg :name :reader belief-system-name))
  (:documentation "Base class for pluggable belief algebras."))

(defvar *belief-system* nil
  "The currently active belief system. Bind or setf to switch algebras.")

;;; --- Protocol generic functions ---

(defgeneric valid-belief-p (system value)
  (:documentation "Returns T if VALUE is a valid belief for this system."))

(defgeneric normalize-belief (system value)
  (:documentation "Convert external value to internal representation.
   Used when asserting facts with :belief keyword.")
  (:method ((system belief-system) value) value))

(defgeneric combine-beliefs (system belief-1 belief-2)
  (:documentation "Combine two independent beliefs supporting the same hypothesis.
   Called when multiple rules conclude the same fact."))

(defgeneric weaken-belief (system belief factor)
  (:documentation "Apply a weakening factor (rule's intrinsic belief) to evidence.
   FACTOR is typically the rule's :belief value."))

(defgeneric conjoin-beliefs (system beliefs)
  (:documentation "Combine beliefs from AND-ed rule premises.
   BELIEFS is a list of belief values from matched facts."))

(defgeneric belief->number (system belief)
  (:documentation "Extract a single representative number for sorting/display.
   Used by conflict resolution strategies and simple displays.")
  (:method ((system belief-system) (belief number)) belief)
  (:method ((system belief-system) (belief null)) 0.0))

(defgeneric belief->english (system belief)
  (:documentation "Human-readable description of the belief."))

(defgeneric belief->json (system belief)
  (:documentation "JSON-serializable representation for the bridge.")
  (:method ((system belief-system) (belief number)) belief)
  (:method ((system belief-system) (belief null)) nil))

(defgeneric default-belief (system)
  (:documentation "Default belief for facts asserted without explicit :belief.")
  (:method ((system belief-system)) nil))


;;; --- Dispatcher functions (called by Lisa core) ---

(defun adjust-belief* (matched-facts rule-belief old-belief)
  "Compute updated belief when a rule fires.

   MATCHED-FACTS: list of facts that satisfied the rule's premises
   RULE-BELIEF: the rule's intrinsic belief factor (from :belief keyword)
   OLD-BELIEF: existing belief on the conclusion fact, or nil

   Returns the new belief value to store on the conclusion fact."
  (when (null *belief-system*)
    (return-from adjust-belief* old-belief))
  (let* ((system *belief-system*)
         ;; Extract beliefs from matched premise facts
         (premise-beliefs (remove nil (mapcar #'belief-factor matched-facts)))
         ;; Combine premises (AND semantics)
         (conjoined (when premise-beliefs
                      (conjoin-beliefs system premise-beliefs)))
         ;; Apply rule's intrinsic weight
         (new-belief (cond
                       ((and conjoined rule-belief)
                        (weaken-belief system conjoined rule-belief))
                       (rule-belief
                        (normalize-belief system rule-belief))
                       (conjoined conjoined)
                       (t nil))))
    ;; Combine with existing conclusion belief
    (if (and old-belief new-belief)
        (combine-beliefs system old-belief new-belief)
        (or new-belief old-belief))))


;;; ============================================================
;;; Part 2: Certainty Factors Implementation (backward compat)
;;; ============================================================
;;; File: src/belief-systems/certainty-factors.lisp

(in-package :belief)

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
         (+ a b (* -1.0 a b)))            ; both positive -> asymptotic to 1
        ((and (minusp a) (minusp b))
         (+ a b (* a b)))                  ; both negative -> asymptotic to -1
        (t                                 ; conflicting
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

(defvar *cf-system* (make-instance 'certainty-factor-system)
  "Singleton certainty factor system instance.")


;;; ============================================================
;;; Part 3: Simplified Dempster-Shafer Implementation
;;; ============================================================
;;; File: src/belief-systems/dempster-shafer.lisp

(in-package :belief)

;;; --- Belief representation ---

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


;;; --- Belief system class ---

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
            ;; Positive: evidence for. Bel = value, Pl = 1.0
            (make-ds-belief (coerce value 'single-float) 1.0))
           (t
            ;; Negative: evidence against. Bel = 0, Pl = 1 + value
            (make-ds-belief 0.0 (coerce (+ 1.0 value) 'single-float)))))))


;;; --- Core algebra ---

(defmethod combine-beliefs ((system dempster-shafer-system) a b)
  "Combine two DS beliefs for the same hypothesis.

   This is a simplified combination that:
   - Combines beliefs using CF-style asymptotic formula (lower bound)
   - Takes minimum plausibility (conservative upper bound)

   Note: Full DS would require mass functions over the power set.
   This approximation works well for single-hypothesis reasoning."
  (let ((bel-a (ds-belief-bel a))
        (bel-b (ds-belief-bel b))
        (pl-a (ds-belief-pl a))
        (pl-b (ds-belief-pl b)))
    ;; Belief combination: asymptotic toward 1 (like positive CF)
    (let ((combined-bel (+ bel-a bel-b (* -1.0 bel-a bel-b)))
          ;; Plausibility: conservative (minimum)
          ;; If either source rules it out, combined should reflect that
          (combined-pl (min pl-a pl-b)))
      ;; Ensure bel <= pl (can happen if evidence strongly supports
      ;; but other evidence rules out)
      (make-ds-belief (min combined-bel combined-pl) combined-pl))))

(defmethod weaken-belief ((system dempster-shafer-system) belief factor)
  "Apply rule weight to belief.

   Scales belief proportionally; plausibility moves toward 1.0
   (weaker evidence = more uncertainty)."
  (let* ((bel (ds-belief-bel belief))
         (pl (ds-belief-pl belief))
         (factor (coerce factor 'single-float))
         ;; Scale belief down
         (new-bel (* bel factor))
         ;; Plausibility: interpolate toward 1.0 as factor decreases
         ;; At factor=1.0, pl unchanged. At factor=0.0, pl=1.0
         (new-pl (+ (* pl factor) (* 1.0 (- 1.0 factor)))))
    (make-ds-belief new-bel (max new-bel new-pl))))

(defmethod conjoin-beliefs ((system dempster-shafer-system) beliefs)
  "Combine beliefs from AND-ed premises.

   AND semantics:
   - Belief is minimum (weakest link, like CF)
   - Plausibility is minimum (if any premise is doubtful, conclusion is)"
  (make-ds-belief
   (apply #'min (mapcar #'ds-belief-bel beliefs))
   (apply #'min (mapcar #'ds-belief-pl beliefs))))


;;; --- Display and serialization ---

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
         ;; High belief, low ignorance = confident
         ((and (> bel 0.7) (< ignorance 0.2))
          (format nil "strong evidence (~,0F%)" (* 100 bel)))
         ;; High belief but high ignorance = suggestive but uncertain
         ((and (> bel 0.5) (>= ignorance 0.3))
          (format nil "suggestive (~,0F%) but uncertain" (* 100 bel)))
         ;; Moderate belief
         ((> bel 0.3)
          (format nil "moderate evidence (~,0F%)" (* 100 bel)))
         ;; Low belief, low plausibility = ruled out
         ((< pl 0.3)
          (format nil "largely ruled out (plausibility ~,0F%)" (* 100 pl)))
         ;; Low belief, high plausibility = little evidence either way
         ((and (< bel 0.2) (> pl 0.8))
          "insufficient evidence")
         ;; Default
         (t
          (format nil "belief ~,0F-~,0F%%" (* 100 bel) (* 100 pl))))))))

(defmethod belief->json ((system dempster-shafer-system) belief)
  "JSON representation: object with bel and pl fields."
  (cond
    ((null belief) nil)
    ((ds-belief-p belief)
     ;; Return alist for jzon to serialize
     `(("bel" . ,(ds-belief-bel belief))
       ("pl" . ,(ds-belief-pl belief))
       ("ignorance" . ,(ds-ignorance belief))))
    (t belief)))


;;; --- Singleton ---

(defvar *ds-system* (make-instance 'dempster-shafer-system)
  "Singleton simplified Dempster-Shafer system instance.")


;;; ============================================================
;;; Part 4: Integration with Lisa Core
;;; ============================================================
;;; Changes needed in src/core/rete.lisp

#|
Replace the current adjust-belief methods with:

(defmethod adjust-belief (rete fact (belief-factor t))
  "Adjust belief on a fact using the active belief system."
  (declare (ignore rete))
  (when (and (in-rule-firing-p) belief:*belief-system*)
    (let ((rule-belief (belief-factor (active-rule)))
          (facts (token-make-fact-list *active-tokens*)))
      (setf (belief-factor fact)
            (belief:adjust-belief* facts rule-belief (belief-factor fact))))))

(defmethod adjust-belief (rete fact (belief-factor number))
  "Explicit belief assignment (normalize through active system)."
  (with-unique-fact (rete fact)
    (setf (belief-factor fact)
          (if belief:*belief-system*
              (belief:normalize-belief belief:*belief-system* belief-factor)
              belief-factor))))
|#


;;; ============================================================
;;; Part 5: Bridge Handler Updates
;;; ============================================================
;;; Changes needed in src/llm/bridge/handlers.lisp

#|
In the /conclusions endpoint, update belief extraction:

(defun format-conclusion (fact)
  "Format a conclusion fact for JSON response."
  (let* ((belief-val (belief:belief-factor fact))
         (system belief:*belief-system*)
         (belief-json (if system
                          (belief:belief->json system belief-val)
                          belief-val))
         (belief-english (if system
                             (belief:belief->english system belief-val)
                             nil)))
    `(("organism" . ,(get-slot-value fact 'value))
      ("entity" . ,(format nil "~A" (get-slot-value fact 'entity)))
      ("belief" . ,belief-json)
      ("belief_english" . ,belief-english))))

In /assert-fact, handle DS belief input:

;; When receiving confidence from the LLM:
(let ((belief-input (or (gethash "confidence" json-data)
                        (gethash "belief" json-data))))
  ;; Could be a number or {"bel": 0.5, "pl": 0.8}
  (when belief-input
    (setf belief-value
          (if (and (hash-table-p belief-input) belief:*belief-system*)
              ;; Parse DS belief from JSON
              (belief:make-ds-belief
               (gethash "bel" belief-input 0.0)
               (gethash "pl" belief-input 1.0))
              ;; Simple number
              belief-input))))
|#


;;; ============================================================
;;; Part 6: System Prompt Updates for Claude
;;; ============================================================
;;; Additions to src/llm/claude/system-prompt.md

#|
Add section on belief interpretation:

## Understanding Belief Values

The diagnostic engine returns belief as an interval [bel, pl]:

- **bel** (belief): Evidence that directly supports this diagnosis
- **pl** (plausibility): Maximum possible support given current evidence
- **ignorance** = pl - bel: How much we don't know yet

### Interpreting Results

| Pattern | Meaning | Next Action |
|---------|---------|-------------|
| bel=0.7, pl=0.8 | Strong evidence, low uncertainty | May be ready to conclude |
| bel=0.4, pl=0.9 | Some evidence, high uncertainty | Need discriminating questions |
| bel=0.1, pl=0.3 | Largely ruled out | Deprioritize this hypothesis |
| bel=0.0, pl=1.0 | No evidence either way | Need more information |

### Narrating to Patients

- Don't quote raw numbers
- Use natural language: "The evidence suggests X, though we should confirm with Y"
- When ignorance is high, emphasize need for more information
- When multiple hypotheses have similar intervals, explain the differential
|#


;;; ============================================================
;;; Part 7: Example Usage
;;; ============================================================

#|
;; Initialize with DS system
(setf belief:*belief-system* belief:*ds-system*)

;; Or switch back to CF
(setf belief:*belief-system* belief:*cf-system*)

;; Assert fact with numeric belief (gets normalized)
(assert (gram (value neg) (entity ?organism)) :belief 0.8)
;; -> DS: [0.8, 1.0]

;; Assert evidence against (negative number)
(assert (gram (value pos) (entity ?organism)) :belief -0.7)
;; -> DS: [0.0, 0.3] — low plausibility

;; Assert with explicit DS belief (if API supports)
(assert (aerobicity (value aerobic) (entity ?org))
        :belief (belief:make-ds-belief 0.6 0.85))
;; -> DS: [0.6, 0.85]

;; After running inference, check conclusion
(dolist (fact (get-conclusions))
  (let ((belief (belief:belief-factor fact)))
    (format t "~A: ~A~%"
            (get-slot-value fact 'value)
            (belief:belief->english belief:*ds-system* belief))))

;; Example output:
;; pseudomonas: suggestive (52%) but uncertain
;; enterobacteriaceae: strong evidence (71%)
;; bacteroides: largely ruled out (plausibility 15%)
|#


;;; ============================================================
;;; Part 8: Testing Scenarios
;;; ============================================================

#|
Test 1: Compare CF vs DS on culture-1 scenario

;; With CF
(setf belief:*belief-system* belief:*cf-system*)
(culture-1)
;; Expected: pseudomonas 0.6, enterobacteriaceae 0.8

;; With DS
(setf belief:*belief-system* belief:*ds-system*)
(culture-1)
;; Expected: pseudomonas [0.6, 1.0], enterobacteriaceae [0.8, 1.0]
;; (No ruling-out evidence in this scenario, so pl stays at 1.0)

Test 2: Conflicting evidence (culture-2 scenario)

;; Culture-2 has both gram-neg (0.8) and gram-pos (0.2)
;; With CF: potential weirdness in combination
;; With DS: gram-neg [0.8, 1.0], gram-pos [0.2, 1.0]
;;          Both remain plausible, but gram-neg has more evidence

Test 3: Evidence that rules out

;; Add a fact that rules out anaerobic organisms
(assert (aerobicity (value aerobic) (entity ?org)) :belief 0.9)
;; This should lower plausibility of bacteroides (anaerobic)
;; Need rule: "if aerobic, then not-anaerobic-organisms"

;; With explicit rule:
(defrule aerobic-rules-out-anaerobes (:belief 0.85)
  (aerobicity (value aerobic) (entity ?organism))
  (organism-identity (value ?org-type) (entity ?organism))
  (test (member ?org-type '(bacteroides)))
  =>
  ;; Assert negative evidence
  (assert (organism-identity (value ?org-type) (entity ?organism)) :belief -0.85))

;; Result: bacteroides plausibility drops to ~0.15
|#


;;; ============================================================
;;; Part 9: Open Questions
;;; ============================================================

#|
1. STORAGE FORMAT
   Current: belief slot holds any value
   Question: Should we add a belief-type slot for runtime dispatch?
   Recommendation: No — use *belief-system* for interpretation

2. COMBINATION SEMANTICS
   Current prototype: asymptotic belief, min plausibility
   Question: Is this faithful enough to DS semantics?
   Recommendation: Test on MYCIN scenarios, refine if needed

3. RULE SYNTAX FOR RULING-OUT
   Current: negative :belief on rules
   Question: Should we add explicit :rules-out syntax?
   Example: (defrule aerobic-rules-out (:rules-out (bacteroides clostridium)))
   Recommendation: Defer — can express with negative beliefs for now

4. BRIDGE INPUT FORMAT
   Current: confidence as number
   Question: Should LLM be able to specify [bel, pl] directly?
   Recommendation: Yes, accept {"bel": x, "pl": y} or just number

5. WHEN TO NORMALIZE IGNORANCE
   Full DS normalizes after combination (the K factor)
   Question: Do we need explicit normalization?
   Recommendation: Not in simplified version — revisit if results look wrong
|#
