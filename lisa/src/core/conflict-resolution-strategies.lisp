;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: strategies.lisp
;;; Description: Classes that implement the various default conflict
;;; resolution strategies for Lisa's RETE implementation.

;;; $Id: conflict-resolution-strategies.lisp,v 1.3 2007/09/11 21:14:09 youngde Exp $

(in-package "LISA")

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
