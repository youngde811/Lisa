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

;; Description: Pluggable belief system protocol. Defines the generic function
;; interface that all belief algebras must implement, and the dispatcher that
;; Lisa's core calls when rules fire.

(in-package :belief)

;;; ============================================================
;;; Belief System Base Class
;;; ============================================================

(defclass belief-system ()
  ((name :initarg :name :reader belief-system-name))
  (:documentation "Base class for pluggable belief algebras."))

(defvar *belief-system* nil
  "The currently active belief system. Bind or setf to switch algebras.")

;;; ============================================================
;;; Protocol Generic Functions
;;; ============================================================

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

;;; ============================================================
;;; Dispatcher (called by Lisa core)
;;; ============================================================

;;; ============================================================
;;; Bridging: connect the old adjust-belief generic (called by
;;; Lisa core) to the new pluggable protocol.
;;; ============================================================

(defmethod adjust-belief (objects (rule-belief number) &optional (old-belief nil))
  (adjust-belief* objects rule-belief old-belief))

(defmethod adjust-belief (objects (rule-belief t) &optional old-belief)
  (declare (ignore objects old-belief))
  nil)

;;; ============================================================
;;; Dispatcher (called by bridging methods above and available
;;; for direct use by new code)
;;; ============================================================

(defun adjust-belief* (matched-facts rule-belief old-belief)
  "Compute updated belief when a rule fires.

   MATCHED-FACTS: list of facts that satisfied the rule's premises
   RULE-BELIEF: the rule's intrinsic belief factor (from :belief keyword)
   OLD-BELIEF: existing belief on the conclusion fact, or nil

   Returns the new belief value to store on the conclusion fact."
  (let ((updated-belief old-belief))
    (unless (null *belief-system*)
      (let* ((system *belief-system*)
             (premise-beliefs (remove nil (mapcar #'belief-factor matched-facts)))
             (conjoined (when premise-beliefs
                          (conjoin-beliefs system premise-beliefs)))
             (new-belief (cond
                           ((and conjoined rule-belief)
                            (weaken-belief system conjoined rule-belief))
                           (rule-belief
                            (normalize-belief system rule-belief))
                           (conjoined conjoined)
                           (t nil))))
        (setf updated-belief
              (if (and old-belief new-belief)
                  (combine-beliefs system old-belief new-belief)
                  (or new-belief old-belief)))))
    updated-belief))

;;; ============================================================
;;; Convenience API
;;; ============================================================

(defvar *cf-system*)
(defvar *ds-system*)

(defun use-system (name)
  "Set the active belief system by keyword.
   Supported: :certainty-factors, :dempster-shafer"
  (setf *belief-system*
        (ecase name
          (:certainty-factors *cf-system*)
          (:dempster-shafer *ds-system*))))