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

(in-package :lisa)

(defclass rule ()
  ((short-name :initarg :short-name
               :initform nil
               :reader rule-short-name)
   (qualified-name :reader rule-name)
   (comment :initform nil
            :initarg :comment
            :reader rule-comment)
   (salience :initform 0
             :initarg :salience
             :reader rule-salience)
   (context :initarg :context
            :reader rule-context)
   (auto-focus :initform nil
               :initarg :auto-focus
               :reader rule-auto-focus)
   (behavior :initform nil
             :initarg :behavior
             :accessor rule-behavior)
   (binding-set :initarg :binding-set
                :initform nil
                :reader rule-binding-set)
   (node-list :initform nil
              :reader rule-node-list)
   (activations :initform (make-hash-table :test #'equal)
                :accessor rule-activations)
   (patterns :initform (list)
             :initarg :patterns
             :reader rule-patterns)
   (actions :initform nil
            :initarg :actions
            :reader rule-actions)
   (logical-marker :initform nil
                   :initarg :logical-marker
                   :reader rule-logical-marker)
   (belief-factor :initarg :belief
                  :initform nil
                  :reader belief-factor)
   (active-dependencies :initform (make-hash-table :test #'equal)
                        :reader rule-active-dependencies)
   (engine :initarg :engine
           :initform nil
           :reader rule-engine))
  (:documentation
   "Represents production rules after they've been analysed by the language
  parser."))

(defmethod fire-rule ((self rule) tokens)
  (let ((*active-rule* self)
        (*active-engine* (rule-engine self))
        (*active-tokens* tokens))
    (unbind-rule-activation self tokens)
    (funcall (rule-behavior self) tokens)))

(defun rule-default-name (rule)
  (if (initial-context-p (rule-context rule))
      (rule-short-name rule)
    (rule-name rule)))

(defun bind-rule-activation (rule activation tokens)
  (setf (gethash (hash-key tokens) (rule-activations rule))
    activation))

(defun unbind-rule-activation (rule tokens)
  (remhash (hash-key tokens) (rule-activations rule)))

(defun clear-activation-bindings (rule)
  (clrhash (rule-activations rule)))

(defun find-activation-binding (rule tokens)
  (gethash (hash-key tokens) (rule-activations rule)))

(defun attach-rule-nodes (rule nodes)
  (setf (slot-value rule 'node-list) nodes))

(defun compile-rule-behavior (rule actions)
  (with-accessors ((behavior rule-behavior)) rule
    (unless behavior
      (setf (rule-behavior rule)
        (make-behavior (rule-actions-actions actions)
                       (rule-actions-bindings actions))))))

(defmethod conflict-set ((self rule))
  (conflict-set (rule-context self)))

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t)
    (format strm "~A"
            (if (initial-context-p (rule-context self))
                (rule-short-name self)
              (rule-name self)))))

(defun compile-rule (rule patterns actions)
  (compile-rule-behavior rule actions)
  (add-rule-to-network (rule-engine rule) rule patterns)
  rule)

(defun logical-rule-p (rule)
  (numberp (rule-logical-marker rule)))

(defun auto-focus-p (rule)
  (rule-auto-focus rule))

(defun find-any-logical-boundaries (patterns)
  (flet ((ensure-logical-blocks-are-valid (addresses)
           (cl:assert (and (= (first (last addresses)) 1)
                           (eq (parsed-pattern-class (first patterns))
                               'initial-fact)) nil
             "Logical patterns must appear first within a rule.")
           ;; BUG FIX - FEB 17, 2004 - Aneil Mallavarapu
           ;;         - replaced: 
           ;; (reduce #'(lambda (first second) 
           ;; arguments need to be inverted because address values are PUSHed
           ;; onto the list ADDRESSES, and therefore are in reverse order
           (reduce #'(lambda (second first)
                       (cl:assert (= second (1+ first)) nil
                         "All logical patterns within a rule must be contiguous.")
                       second)
                   addresses :from-end t)))
    (let ((addresses (list)))
      (dolist (pattern patterns)
        (when (logical-pattern-p pattern)
          (push (parsed-pattern-address pattern) addresses)))
      (unless (null addresses)
        (ensure-logical-blocks-are-valid addresses))
      (first addresses))))

(defmethod initialize-instance :after ((self rule) &rest initargs)
  (declare (ignore initargs))
  (with-slots ((qual-name qualified-name)) self
    (setf qual-name
      (intern (format nil "~A.~A" 
                      (context-name (rule-context self))
                      (rule-short-name self))))))
                    
(defun make-rule (name engine patterns actions 
                  &key (doc-string nil) 
                       (salience 0) 
                       (context (active-context))
                       (auto-focus nil)
                       (belief nil)
                       (compiled-behavior nil))
  (flet ((make-rule-binding-set ()
           (delete-duplicates
            (loop for pattern in patterns
                append (parsed-pattern-binding-set pattern)))))
    (compile-rule
     (make-instance 'rule 
       :short-name name 
       :engine engine
       :patterns patterns
       :actions actions
       :behavior compiled-behavior
       :comment doc-string
       :belief belief
       :salience salience
       :context (if (null context)
                    (find-context (inference-engine) :initial-context)
                  (find-context (inference-engine) context))
       :auto-focus auto-focus
       :logical-marker (find-any-logical-boundaries patterns)
       :binding-set (make-rule-binding-set))
     patterns actions)))

(defun copy-rule (rule engine)
  (let ((initargs `(:doc-string ,(rule-comment rule)
                    :salience ,(rule-salience rule)
                    :context ,(if (initial-context-p (rule-context rule))
                                  nil
                                (context-name (rule-context rule)))
                    :compiled-behavior ,(rule-behavior rule)
                    :auto-focus ,(rule-auto-focus rule))))
    (with-inference-engine (engine)
      (apply #'make-rule
             (rule-short-name rule)
             engine
             (rule-patterns rule)
             (rule-actions rule)
             initargs))))
