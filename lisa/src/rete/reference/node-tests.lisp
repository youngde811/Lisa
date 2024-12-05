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

;;; File: node-tests.lisp
;;; Description:

;;; $Id: node-tests.lisp,v 1.23 2007/09/11 21:14:10 youngde Exp $

(in-package "LISA")

(defvar *node-test-table*)

(defun find-test (key constructor)
  (let ((test (gethash key *node-test-table*)))
    (when (null test)
      (setf test
        (setf (gethash key *node-test-table*)
          (funcall constructor))))
    test))
  
(defun clear-node-test-table ()
  (clrhash *node-test-table*))

(defmethod class-matches-p ((instance inference-engine-object) fact class)
  (eq (fact-name fact) class))
  
(defmethod class-matches-p ((instance t) fact class)
  (or (eq (fact-name fact) class)
      (has-superclass fact class)))

(defun make-class-test (class)
  (find-test class
             #'(lambda ()
                 (function
                  (lambda (token)
                    (declare (optimize (speed 3) (debug 1) (safety 0)))
                    (let ((fact (token-top-fact token)))
                      (class-matches-p 
                       (find-instance-of-fact fact) fact class)))))))

(defun make-simple-slot-test-aux (slot-name value negated-p)
  (find-test 
   `(,slot-name ,value ,negated-p)
   #'(lambda ()
       (let ((test
              (function
               (lambda (token)
                 (declare (optimize (speed 3) (debug 1) (safety 0)))
                 (equal value
                        (get-slot-value
                         (token-top-fact token)
                         slot-name))))))
         (if negated-p
             (complement test)
           test)))))

(defun make-simple-slot-test (slot)
  (declare (type pattern-slot slot))
  (make-simple-slot-test-aux
   (pattern-slot-name slot)
   (pattern-slot-value slot)
   (pattern-slot-negated slot)))

#+ignore
(defmacro make-variable-test (slot-name binding)
  `(function
    (lambda (tokens)
      (equal (get-slot-value (token-top-fact tokens) ,slot-name)
             (get-slot-value 
              (token-find-fact tokens (binding-address ,binding))
              (binding-slot-name ,binding))))))

(defun make-inter-pattern-test (slot)
  (let* ((binding (pattern-slot-slot-binding slot))
         (test
          (function
           (lambda (tokens)
             (declare (optimize (speed 3) (debug 1) (safety 0)))
             (equal (get-slot-value (token-top-fact tokens)
                                    (pattern-slot-name slot))
                    (get-slot-value
                     (token-find-fact tokens (binding-address binding))
                     (binding-slot-name binding)))))))
    (if (negated-slot-p slot) (complement test) test)))

(defun make-predicate-test (forms bindings &optional (negated-p nil))
  (let* ((special-vars
          (mapcar #'binding-variable bindings))
         (body
          (if (consp (first forms)) 
              forms
            (list forms)))
         (predicate
          (compile nil `(lambda ()
                          (declare (special ,@special-vars))
                          ,@body)))
         (test
          (function
           (lambda (tokens)
             (progv
                 `(,@special-vars)
                 `(,@(mapcar #'(lambda (binding)
                                 (declare (optimize (speed 3) (debug 1) (safety 0)))
                                 (if (pattern-binding-p binding)
                                     (token-find-fact 
                                      tokens (binding-address binding))
                                   (get-slot-value
                                    (token-find-fact 
                                     tokens (binding-address binding))
                                    (binding-slot-name binding))))
                             bindings))
               (funcall predicate))))))
    (if negated-p (complement test) test)))

(defun make-intra-pattern-predicate (forms bindings negated-p)
  (let* ((special-vars
          (mapcar #'binding-variable bindings))
         (body
          (if (consp (first forms)) 
              forms
            (list forms)))
         (predicate
          (compile nil `(lambda ()
                          (declare (special ,@special-vars))
                          (declare (optimize (speed 3) (debug 1) (safety 0)))
                          ,@body)))
         (test
          (function
           (lambda (tokens)
             (progv
                 `(,@special-vars)
                 `(,@(mapcar #'(lambda (binding)
                                 (declare (optimize (speed 3) (debug 1) (safety 0)))
                                 (if (pattern-binding-p binding)
                                     (token-find-fact 
                                      tokens (binding-address binding))
                                   (get-slot-value
                                    (token-top-fact tokens)
                                    (binding-slot-name binding))))
                             bindings))
               (funcall predicate))))))
    (if negated-p (complement test) test)))
         
(defun make-intra-pattern-constraint-test (slot)
  (make-intra-pattern-predicate
   (pattern-slot-constraint slot)
   (pattern-slot-constraint-bindings slot)
   (negated-slot-p slot)))

(defun make-intra-pattern-test (slot)
  (let ((test
         (function
          (lambda (tokens)
            (declare (optimize (speed 3) (debug 1) (safety 0)))
            (equal (get-slot-value (token-top-fact tokens)
                                   (pattern-slot-name slot))
                   (get-slot-value (token-top-fact tokens)
                                   (binding-slot-name 
                                    (pattern-slot-slot-binding slot))))))))
    (if (negated-slot-p slot) (complement test) test)))

(defun make-behavior (function bindings)
  (make-predicate-test function bindings))
