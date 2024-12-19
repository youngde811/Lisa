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
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (token-push-fact left-tokens (token-top-fact right-token))
  (prog1
      (every #'(lambda (test)
                 (declare (type function test))
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
