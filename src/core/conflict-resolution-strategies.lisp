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

;; Description: Classes that implement the various default conflict resolution
;; strategies for Lisa's RETE implementation.

(in-package :lisa)

(defclass strategy ()
  ()
  (:documentation
   "Serves as the base class for all classes implementing conflict
   resolution strategies."))

(defgeneric add-activation (strategy activation))
(defgeneric find-activation (strategy rule token))
(defgeneric find-all-activations (strategy rule))
(defgeneric next-activation (strategy))
(defgeneric remove-activations (strategy))
(defgeneric list-activations (strategy))

(defclass priority-queue-mixin ()
  ((heap :initarg :heap
         :reader heap)))

(defmethod reset-activations ((self priority-queue-mixin))
  (heap:heap-clear (heap self)))

(defmethod insert-activation ((self priority-queue-mixin) activation)
  (heap:heap-insert (heap self) activation))

(defmethod lookup-activation ((self priority-queue-mixin) rule tokens)
  (heap:heap-find (heap self) #'(lambda (heap activation)
                                  (declare (ignore heap))
                                  (and (equal (hash-key activation) (hash-key tokens))
                                       (eq (activation-rule activation) rule)))))

(defmethod lookup-activations ((self priority-queue-mixin) rule)
  (heap:heap-collect (heap self) #'(lambda (heap activation)
                                     (declare (ignore heap))
                                     (and activation
                                          (eq rule (activation-rule activation))))))

(defmethod get-next-activation ((self priority-queue-mixin))
  (heap:heap-remove (heap self)))

(defmethod get-all-activations ((self priority-queue-mixin))
  (heap:heap-collect (heap self) (lambda (heap activation)
                                   (declare (ignore heap))
                                   activation)))

(defclass builtin-strategy (strategy priority-queue-mixin)
  ()
  (:documentation
   "A base class for all LISA builtin conflict resolution strategies."))
  
(defmethod add-activation ((self builtin-strategy) activation)
  (insert-activation self activation))

(defmethod find-activation ((self builtin-strategy) rule token)
  (declare (ignore rule token))
  (cl:assert nil nil "Why are we calling FIND-ACTIVATION?"))

(defmethod find-all-activations ((self builtin-strategy) rule)
  (lookup-activations self rule))

(defmethod next-activation ((self builtin-strategy))
  (get-next-activation self))

(defmethod remove-activations ((self builtin-strategy))
  (reset-activations self))

(defmethod list-activations ((self builtin-strategy))
  (get-all-activations self))

(defclass depth-first-strategy (builtin-strategy)
  ()
  (:documentation
   "A depth-first conflict resolution strategy."))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy
                 :heap (heap:create-heap #'(lambda (a b)
                                             (cond ((> (activation-priority a)
                                                       (activation-priority b))
                                                    a)
                                                   ((and (= (activation-priority a)
                                                            (activation-priority b))
                                                         (> (activation-timestamp a)
                                                            (activation-timestamp b)))
                                                    a)
                                                   (t nil))))))

(defclass breadth-first-strategy (builtin-strategy)
  ()
  (:documentation
   "A breadth-first conflict resolution strategy."))

(defun make-breadth-first-strategy ()
  (make-instance 'breadth-first-strategy
                 :heap (heap:create-heap #'(lambda (a b)
                                             (cond ((> (activation-priority a)
                                                       (activation-priority b))
                                                    a)
                                                   ((and (= (activation-priority a)
                                                            (activation-priority b))
                                                         (< (activation-timestamp a)
                                                            (activation-timestamp b)))
                                                    a)
                                                   (t nil))))))

;;; Test code.

#|
(defvar *heap-timestamp* 0)

(defstruct heap-object
  priority timestamp name)

(defun new-heap-object (priority name)
  (make-heap-object :priority priority :name name :timestamp (incf *heap-timestamp*)))

(defun make-breadth-heap ()
  (create-heap #'(lambda (a b)
                   (cond ((> (heap-object-priority a)
                             (heap-object-priority b))
                          a)
                         ((= (heap-object-priority a)
                             (heap-object-priority b))
                          (< (heap-object-timestamp a)
                             (heap-object-timestamp b)))
                         (t nil)))))
|#
