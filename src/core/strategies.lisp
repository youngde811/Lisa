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

(defclass indexed-priority-list ()
  ((priority-vector :reader get-priority-vector)
   (inodes :initform '()
           :accessor get-inodes)
   (delta :accessor get-delta)
   (insertion-function :initarg :insertion-function
                       :reader get-insertion-function))
  (:documentation
   "Utility class that implements an indexed priority 'queue' to manage
   activations. Employed by various types of conflict resolution strategies,
   particularly DEPTH-FIRST-STRATEGY and BREADTH-FIRST-STRATEGY."))

(defmethod initialize-instance :after ((self indexed-priority-list)
                                       &key (priorities 500))
  (setf (slot-value self 'priority-vector)
    (make-array (1+ priorities) :initial-element nil))
  (setf (slot-value self 'delta) (/ priorities 2)))

(defun reset-activations (self)
  (declare (type indexed-priority-list self))
  (let ((queue (get-priority-vector self)))
    (mapc #'(lambda (inode)
              (setf (aref queue inode) nil))
          (get-inodes self))
    (setf (get-inodes self) '())))

(defun insert-activation (plist activation)
  (declare (type indexed-priority-list plist))
  (flet ((index-salience (priority)
           (declare (type fixnum priority))
           (with-accessors ((inodes get-inodes)) plist
             (setf inodes
               (sort (pushnew priority inodes)
                     #'(lambda (p1 p2) (> p1 p2)))))))
    (with-accessors ((vector get-priority-vector)
                     (activations get-activations)) plist
    (let* ((salience (rule-salience (activation-rule activation)))
           (inode (+ salience (get-delta plist)))
           (queue (aref vector inode)))
      (when (null queue)
        (index-salience inode))
      (setf (aref vector inode)
        (apply (get-insertion-function plist)
               `(,activation ,queue)))))))

(defun lookup-activation (self rule tokens)
  (declare (type indexed-priority-list self))
  (find-if #'(lambda (act)
               (and (equal (hash-key act) (hash-key tokens))
                    (eq (activation-rule act) rule)))
           (aref (get-priority-vector self)
                 (+ (rule-salience rule) (get-delta self)))))

(defun lookup-activations (self rule)
  (declare (type indexed-priority-list self))
  (loop for activation
      in (aref (get-priority-vector self)
               (+ (rule-salience rule) (get-delta self)))
      if (eq rule (activation-rule activation))
      collect activation))

(defun get-next-activation (plist)
  (declare (type indexed-priority-list plist))
  (with-accessors ((inodes get-inodes)
                   (vector get-priority-vector)) plist
    (let ((inode (first inodes)))
      (cond ((null inode) nil)
            (t
             (let ((activation (pop (aref vector inode))))
               (when (null (aref vector inode))
                 (pop inodes))
               activation))))))

(defun get-all-activations (plist)
  (let ((activations (list)))
    (with-accessors ((queue get-priority-vector)) plist
      (mapc #'(lambda (inode)
                (mapc #'(lambda (act)
                          (when (eligible-p act)
                            (push act activations)))
                      (aref queue inode)))
            (get-inodes plist)))
    (nreverse activations)))

(defun make-indexed-priority-list (insert-func)
  (make-instance 'indexed-priority-list
                 :insertion-function insert-func))

(defclass builtin-strategy (strategy)
  ((priority-queue :reader get-priority-queue))
  (:documentation
   "A base class for all Lisa builtin conflict resolution strategies."))
  
(defmethod add-activation ((self builtin-strategy) activation)
  (insert-activation (get-priority-queue self) activation))

(defmethod find-activation ((self builtin-strategy) rule token)
  (cl:assert nil nil "Why are we calling FIND-ACTIVATION?")
  (lookup-activation (get-priority-queue self) rule token))

(defmethod find-all-activations ((self builtin-strategy) rule)
  (lookup-activations (get-priority-queue self) rule))

(defmethod next-activation ((self builtin-strategy))
  (get-next-activation (get-priority-queue self)))

(defmethod remove-activations ((self builtin-strategy))
  (reset-activations (get-priority-queue self)))

(defmethod list-activations ((self builtin-strategy))
  (get-all-activations (get-priority-queue self)))

(defclass depth-first-strategy (builtin-strategy)
  ()
  (:documentation
   "A depth-first conflict resolution strategy."))

(defmethod initialize-instance :after ((self depth-first-strategy) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'priority-queue)
    (make-indexed-priority-list
     #'(lambda (obj place) (push obj place)))))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy))

(defclass breadth-first-strategy (builtin-strategy)
  ((tail :initform nil
         :accessor tail))
  (:documentation
   "A breadth-first conflict resolution strategy."))

(defmethod initialize-instance :after ((self breadth-first-strategy) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'priority-queue)
    (make-indexed-priority-list
     #'(lambda (obj place)
         (with-accessors ((p tail)) self
           (cond ((null place)
                  (setf place (list obj))
                  (setf p place))
                 (t
                  (setf p (nconc p (list obj)))
                  (setf p (cdr p))))
           place)))))

(defun make-breadth-first-strategy ()
  (make-instance 'breadth-first-strategy))

