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
                    (declare (optimize (speed 3) (debug 0) (safety 1)))
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
                 (declare (optimize (speed 3) (debug 0) (safety 1)))
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

(defun make-inter-pattern-test (slot)
  (let* ((binding (pattern-slot-slot-binding slot))
         (test
          (function
           (lambda (tokens)
             (declare (optimize (speed 3) (debug 0) (safety 1)))
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
                                 (declare (optimize (speed 3) (debug 0) (safety 1)))
                                 (if (pattern-binding-p binding)
                                     (token-find-fact 
                                      tokens (binding-address binding))
                                   (get-slot-value
                                    (token-find-fact 
                                     tokens (binding-address binding))
                                    (binding-slot-name binding))))
                             bindings))
               (funcall predicate))))))
    (if negated-p
        (complement test)
      test)))

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
                          (declare (optimize (speed 3) (debug 0) (safety 1)))
                          ,@body)))
         (test
          (function
           (lambda (tokens)
             (progv
                 `(,@special-vars)
                 `(,@(mapcar #'(lambda (binding)
                                 (declare (optimize (speed 3) (debug 0) (safety 1)))
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
            (declare (optimize (speed 3) (debug 0) (safety 1)))
            (equal (get-slot-value (token-top-fact tokens)
                                   (pattern-slot-name slot))
                   (get-slot-value (token-top-fact tokens)
                                   (binding-slot-name 
                                    (pattern-slot-slot-binding slot))))))))
    (if (negated-slot-p slot) (complement test) test)))

(defun make-behavior (function bindings)
  (make-predicate-test function bindings))
