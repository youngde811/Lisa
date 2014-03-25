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

;;; File: join-node.lisp
;;; Description:

;;; $Id: join-node.lisp,v 1.16 2007/09/11 21:14:10 youngde Exp $

(in-package "LISA")

(defclass join-node ()
  ((successor :initform nil
              :accessor join-node-successor)
   (logical-block :initform nil
                  :reader join-node-logical-block)
   (tests :initform (list)
          :accessor join-node-tests)
   (left-memory :initform (make-hash-table :test #'equal)
                :reader join-node-left-memory)
   (right-memory :initform (make-hash-table :test #'equal)
                 :reader join-node-right-memory)))

(defun mark-as-logical-block (join-node marker)
  (setf (slot-value join-node 'logical-block) marker))

(defun logical-block-p (join-node)
  (numberp (join-node-logical-block join-node)))

(defun remember-token (memory token)
  (setf (gethash (hash-key token) memory) token))

(defun forget-token (memory token)
  (remhash (hash-key token) memory))

(defun add-tokens-to-left-memory (join-node tokens)
  (remember-token (join-node-left-memory join-node) tokens))

(defun add-token-to-right-memory (join-node token)
  (remember-token (join-node-right-memory join-node) token))

(defun remove-tokens-from-left-memory (join-node tokens)
  (forget-token (join-node-left-memory join-node) tokens))

(defun remove-token-from-right-memory (join-node token)
  (forget-token (join-node-right-memory join-node) token))

(defun left-memory-count (join-node)
  (hash-table-count (join-node-left-memory join-node)))

(defun right-memory-count (join-node)
  (hash-table-count (join-node-right-memory join-node)))

(defmethod test-tokens ((self join-node) left-tokens right-token)
  (token-push-fact left-tokens (token-top-fact right-token))
  (prog1
      (every #'(lambda (test)
                 (funcall test left-tokens))
             (join-node-tests self))
    (token-pop-fact left-tokens)))

(defmethod pass-tokens-to-successor ((self join-node) left-tokens)
  (call-successor (join-node-successor self) left-tokens))

(defmethod combine-tokens ((left-tokens token) (right-token token))
  (token-push-fact (replicate-token left-tokens) (token-top-fact right-token)))

(defmethod combine-tokens ((left-tokens token) (right-token t))
  (token-push-fact (replicate-token left-tokens) right-token))

(defmethod add-successor ((self join-node) successor-node connector)
  (setf (join-node-successor self)
    (make-successor successor-node connector)))

(defmethod join-node-add-test ((self join-node) test)
  (push test (join-node-tests self)))

(defmethod clear-memories ((self join-node))
  (clrhash (join-node-left-memory self))
  (clrhash (join-node-right-memory self)))

(defmethod accept-tokens-from-left ((self join-node) (left-tokens reset-token))
  (clear-memories self)
  (pass-tokens-to-successor self left-tokens))

(defmethod accept-token-from-right ((self join-node) (left-tokens reset-token))
  nil)

(defmethod print-object ((self join-node) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "left ~S ; right ~S ; tests ~S"
            (left-memory-count self)
            (right-memory-count self)
            (length (join-node-tests self)))))
