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

;; Description: The Lisa rule parser, completely rewritten for release 3.0.

(in-package :lisa)

(defconstant RULE-SEPARATOR '=>)

(defvar *binding-table*)
(defvar *current-defrule*)
(defvar *current-defrule-pattern-location*)
(defvar *in-logical-pattern-p* nil)
(defvar *special-initial-elements* '(not exists logical))

;;; (defvar *special-initial-elements* '(logical))

(defvar *conditional-elements-table*
  '((exists . parse-exists-pattern)
    (not . parse-not-pattern)
    (test . parse-test-pattern)))

(defun extract-rule-headers (body)
  (if (stringp (first body))
      (values (first body) (rest body))
    (values nil body)))

(defun preprocess-left-side (lhs)
  (when (or (null lhs)
            (find (caar lhs) *special-initial-elements*))
    (push (list 'initial-fact) lhs))
  lhs)

(defun find-conditional-element-parser (symbol)
  (let ((parser (assoc symbol *conditional-elements-table*)))
    (if parser
        (cdr parser)
      'parse-generic-pattern)))

(defun logical-element-p (pattern)
  (eq (first pattern) 'logical))

(defmacro with-slot-components ((slot-name slot-value constraint) form &body body)
  `(progn
     (unless (consp ,form)
       (error 'slot-parsing-error :slot-name ',slot-name :location *current-defrule-pattern-location*))
     (let ((,slot-name (first ,form))
           (,slot-value (second ,form))
           (,constraint (third ,form)))
       ,@body)))

(defun make-binding-set ()
  (loop for binding being the hash-values of *binding-table*
      collect binding))

(defun find-or-set-slot-binding (var slot-name location)
  "Given a variable, either retrieve the binding object for it or create a new one."
  (multiple-value-bind (binding existsp)
      (gethash var *binding-table*)
    (unless existsp
      (setf binding
        (setf (gethash var *binding-table*)
          (make-binding var location slot-name))))
    (values binding existsp)))

(defun find-slot-binding (var &key (errorp t))
  "Given a variable, retrieve the binding object for it."
  (let ((binding (gethash var *binding-table*)))
    (unless binding
      (when errorp
        (log:error "Missing slot binding for variable ~A" var)
        (error :unrecoverable)))
    binding))

(defun set-pattern-binding (var location)
  (unless (not (gethash var *binding-table*))
    (log:error "This is a duplicate pattern binding: ~A" var)
    (error :unrecoverable))
  (setf (gethash var *binding-table*)
        (make-binding var location :pattern)))

(defun collect-bindings (forms &key (errorp t))
  (let ((bindings (list)))
    (dolist (obj (utils:flatten forms))
      (when (variablep obj)
        (let ((binding (find-slot-binding obj :errorp errorp)))
          (unless (null binding)
            (push binding bindings)))))
    (nreverse bindings)))

(defmacro with-rule-components (((doc-string lhs rhs) rule-form) &body body)
  (let ((remains (gensym)))
    `(let ((*binding-table* (make-hash-table)))
       (multiple-value-bind (,doc-string ,remains)
           (extract-rule-headers ,rule-form)
         (multiple-value-bind (,lhs ,rhs)
             (parse-rule-body ,remains)
           ,@body)))))

(defun collect-constraint-bindings (constraint)
  (let ((bindings (list)))
    (dolist (obj (utils:flatten constraint))
      (when (variablep obj)
        (pushnew (find-slot-binding obj) bindings :key #'first)))
    bindings))

;;; the parsing code itself...

(defun parse-one-slot-constraint (var constraint-form)
  "Parses a single slot constraint, eg. (slot-name ?var 1) or (slot-name ?var (equal ?var 1))"
  (let ((head (first constraint-form))
        (args (second constraint-form)))
    (cond ((eq head 'not)
           (values `(equal ,var ,@(if (symbolp args) `(',args) args))
                   `(,(find-slot-binding var)) t))
          (t
           (values constraint-form (collect-constraint-bindings constraint-form) nil)))))

(defun slot-value-is-variable-p (value)
  "Is the slot value a Lisa variable?"
  (variable-p value))

(defun slot-value-is-atom-p (value)
  "Is the slot value a simple constraint?"
  (and (atom value)
       (not (slot-value-is-variable-p value))))

(defun slot-value-is-negated-atom-p (value)
  "Is the slot value a simple negated constraint?"
  (and (consp value)
       (eq (first value) 'not)
       (slot-value-is-atom-p (second value))))

(defun slot-value-is-negated-variable-p (value)
  (and (consp value)
       (eq (first value) 'not)
       (variable-p (second value))))

(defun intra-pattern-bindings-p (bindings location)
  "Is every variable in a pattern 'local'; i.e. does not reference a binding in a previous pattern?"
  (every #'(lambda (b)
             (= location (binding-address b)))
         bindings))

(defun parse-one-slot (form location)
  "Parses a single raw pattern slot"
  (with-slot-components (slot-name slot-value constraint) form
    (cond ((slot-value-is-atom-p slot-value)
           ;; eg. (slot-name "frodo")
           (make-pattern-slot :name slot-name :value slot-value))
          ((slot-value-is-negated-variable-p slot-value)
           ;; eg. (slot-name (not ?value))
           (let ((binding (find-or-set-slot-binding (second slot-value) slot-name location)))
             (make-pattern-slot :name slot-name
                                :value (second slot-value)
                                :negated t
                                :slot-binding binding)))
          ((slot-value-is-negated-atom-p slot-value)
           ;; eg. (slot-name (not "frodo"))
           (make-pattern-slot :name slot-name :value (second slot-value) :negated t))
          ((and (slot-value-is-variable-p slot-value)
                (not constraint))
           ;; eg. (slot-name ?value)
           (let ((binding (find-or-set-slot-binding slot-value slot-name location)))
             (make-pattern-slot :name slot-name :value slot-value :slot-binding binding
                                :intra-pattern-bindings (intra-pattern-bindings-p (list binding) location))))
          ((and (slot-value-is-variable-p slot-value)
                constraint)
           ;; eg. (slot-name ?value (equal ?value "frodo"))
           (let ((binding (find-or-set-slot-binding slot-value slot-name location)))
             (multiple-value-bind (constraint-form constraint-bindings negatedp)
                 (parse-one-slot-constraint slot-value constraint)
               (make-pattern-slot :name slot-name :value slot-value :slot-binding binding
                                  :negated negatedp
                                  :constraint constraint-form
                                  :constraint-bindings constraint-bindings
                                  :intra-pattern-bindings
                                  (intra-pattern-bindings-p (list* binding constraint-bindings) location)))))
          (t (error 'rule-parsing-error :rule-name *current-defrule*
                    :location *current-defrule-pattern-location*
                    :text "malformed slot")))))

(defun parse-rule-body (body)
  (let ((location 0)
        (patterns (list)))
    (labels ((parse-lhs (pattern-list)
               (let ((pattern (first pattern-list))
                     (*current-defrule-pattern-location* location))
                 (unless (listp pattern)
                   (error 'rule-parsing-error
                          :text "pattern is not a list"
                          :rule-name *current-defrule*
                          :location *current-defrule-pattern-location*))
                 (cond ((null pattern-list)
                        (reverse patterns))
                       ;; logical CEs are "special"; they don't have their own parser.
                       ((logical-element-p pattern)
                        (let ((*in-logical-pattern-p* t))
                          (parse-lhs (rest pattern))))
                       (t
                        (push (funcall (find-conditional-element-parser (first pattern)) pattern
                                       (1- (incf location)))
                              patterns)
                        (parse-lhs (rest pattern-list))))))
             (parse-rhs (actions)
               (make-rule-actions
                :bindings (collect-bindings actions :errorp nil)
                :actions actions)))
      (multiple-value-bind (lhs remains)
          (utils:find-before RULE-SEPARATOR body :test #'eq)
        (unless remains
          (error 'rule-parsing-error :text "missing rule separator"))
        (values (parse-lhs (preprocess-left-side lhs))
                (parse-rhs (utils:find-after RULE-SEPARATOR remains :test #'eq)))))))

;;; The conditional element parsers...

(defun parse-generic-pattern (pattern location &optional pattern-binding)
  (let ((head (first pattern)))
    (unless (symbolp head)
      (error 'rule-parsing-error :rule-name *current-defrule*
             :location *current-defrule-pattern-location*
             :text "the head of a pattern must be a symbol"))
    (cond ((variable-p head)
           (set-pattern-binding head location)
           (parse-generic-pattern (second pattern) location head))
          (t
           (let ((slots
                  (loop for slot-decl in (rest pattern) collect
                        (parse-one-slot slot-decl location))))
             (make-parsed-pattern :type :generic
                                  :pattern-binding pattern-binding
                                  :slots slots
                                  :binding-set (make-binding-set)
                                  :logical *in-logical-pattern-p*
                                  :address location
                                  :class head))))))

(defun parse-test-pattern (pattern location)
  (flet ((extract-test-pattern ()
           (let ((form (rest pattern)))
             (unless (and (listp form)
                          (= (length form) 1))
               (error 'rule-parsing-error
                      :rule-name *current-defrule*
                      :location *current-defrule-pattern-location*
                      :text "TEST takes a single Lisp form as argument"))
             form)))
    (let* ((form (extract-test-pattern))
           (bindings (collect-bindings form)))
      (make-parsed-pattern :test-bindings bindings
                           :type :test
                           :slots form
                           :pattern-binding nil
                           :binding-set (make-binding-set)
                           :logical *in-logical-pattern-p*
                           :address location))))

(defun parse-exists-pattern (pattern location)
  (let ((pattern (parse-generic-pattern (second pattern) location)))
    (setf (parsed-pattern-type pattern) :existential)
    pattern))

(defun parse-not-pattern (pattern location)
  (let ((pattern (parse-generic-pattern (second pattern) location)))
    (setf (parsed-pattern-type pattern) :negated)
    pattern))

;;; High-level rule definition interfaces...

(defun define-rule (name body &key (salience 0) (context nil) (auto-focus nil) (belief nil))
  (let ((*current-defrule* name))
    (with-rule-components ((doc-string lhs rhs) body)
      (make-rule name (inference-engine) lhs rhs
                 :doc-string doc-string
                 :salience salience
                 :context context
                 :belief belief
                 :auto-focus auto-focus))))

(defun redefine-defrule (name body &key (salience 0) (context nil) (belief nil) (auto-focus nil))
  (define-rule name body :salience salience
               :context context
               :belief belief
               :auto-focus auto-focus))

