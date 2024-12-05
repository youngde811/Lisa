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

;;; File: token.lisp
;;; Description:

;;; $Id: token.lisp,v 1.3 2007/09/11 21:14:09 youngde Exp $

(in-package "LISA")

(defclass token ()
  ((facts :initform
          (make-array 0 :adjustable t :fill-pointer t)
          :accessor token-facts)
   (not-counter :initform 0
                :accessor token-not-counter)
   (exists-counter :initform 0
                   :accessor token-exists-counter)
   (hash-code :initform (list)
              :accessor token-hash-code)
   (contents :initform nil
             :reader token-contents)))

(defclass add-token (token) ())
(defclass remove-token (token) ())
(defclass reset-token (token) ())

(defun token-increment-exists-counter (token)
  (incf (token-exists-counter token)))

(defun token-decrement-exists-counter (token)
  (cl:assert (plusp (token-exists-counter token)) nil
    "The EXISTS join node logic is busted.")
  (decf (token-exists-counter token)))

(defun token-increment-not-counter (token)
  (values token (incf (token-not-counter token))))

(defun token-decrement-not-counter (token)
  (cl:assert (plusp (token-not-counter token)) nil
    "The negated join node logic is busted.")
  (values token (decf (token-not-counter token))))

(defun token-negated-p (token)
  (plusp (token-not-counter token)))

(defun token-make-fact-list (token &key (detailp t) (debugp nil))
  (let* ((facts (list))
         (vector (token-facts token))
         (length (length vector)))
    (dotimes (i length)
      (let ((fact (aref vector i)))
        (if debugp
            (push fact facts)
          (when (typep fact 'fact)
            (push (if detailp fact (fact-symbolic-id fact)) 
                  facts)))))
    (nreverse facts)))

(defun token-fact-count (token)
  (length (token-facts token)))

(defun token-find-fact (token address)
  (aref (slot-value token 'facts) address))

(defun token-top-fact (token)
  (declare (optimize (speed 3) (debug 1) (safety 0)))
  (with-slots ((fact-vector facts)) token
    (aref fact-vector (1- (length fact-vector)))))

(defun token-push-fact (token fact)
  (declare (optimize (speed 3) (debug 1) (safety 0)))
  (with-accessors ((fact-vector token-facts)
                   (hash-code token-hash-code)) token
    (vector-push-extend fact fact-vector)
    (push fact hash-code)
    token))

(defun token-pop-fact (token)
  (with-accessors ((fact-vector token-facts)
                   (hash-code token-hash-code)) token
    (unless (zerop (fill-pointer fact-vector))
      (pop hash-code)
      (aref fact-vector (decf (fill-pointer fact-vector))))))

(defun replicate-token (token &key (token-class nil))
  (declare (optimize (speed 3) (safety 0) (debug 1)))
  (let ((new-token 
         (make-instance (if token-class
                            (find-class token-class)
                          (class-of token)))))
    (with-slots ((existing-fact-vector facts)) token
      (let ((length (length existing-fact-vector)))
        (dotimes (i length)
          (token-push-fact new-token (aref existing-fact-vector i)))))
    new-token))

(defmethod hash-key ((self token))
  (token-hash-code self))

(defmethod make-add-token ((fact fact))
  (token-push-fact (make-instance 'add-token) fact))

(defmethod make-remove-token ((fact fact))
  (token-push-fact (make-instance 'remove-token) fact))

(defmethod make-remove-token ((token token))
  (replicate-token token :token-class 'remove-token))

(defmethod make-reset-token ((fact t))
  (token-push-fact (make-instance 'reset-token) t))
