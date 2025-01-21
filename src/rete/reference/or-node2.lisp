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

(defclass or-node2 (node2)
  ((branch-successes :initform (make-hash-table :test #'equal) 
                     :accessor branch-successes)
   (branch-tokens :initform (make-hash-table :test #'equal)
                  :accessor branch-tokens)))

(defmethod test-tokens ((self or-node) left-tokens right-token)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (token-push-fact left-tokens (token-top-fact right-token))
  (prog1
      (some #'(lambda (test)
                (declare (type function test))
                (funcall test left-tokens))
            (join-node-tests self))
    (token-pop-fact left-tokens)))

;; Helper to create a unique key for a token in a specific branch

(defun make-branch-key (token branch-index)
  (cons (token-facts token) branch-index))

(defmethod test-against-left-memory ((self or-node) (right-token add-token))
  (loop for left-tokens being the hash-values of (join-node-left-memory self)
        for branch-index from 0
        when (test-tokens self left-tokens right-token)
        do (let ((branch-key (make-branch-key left-tokens branch-index)))
             (setf (gethash branch-key (or-node-branch-successes self)) t)
             (setf (gethash branch-key (or-node-branch-tokens self))
                   (combine-tokens left-tokens right-token))
             (pass-tokens-to-successor 
              self (gethash branch-key (or-node-branch-tokens self))))))

(defmethod test-against-right-memory ((self or-node) left-tokens)
  (loop for right-token being the hash-values of (join-node-right-memory self)
        for branch-index from 0
        when (test-tokens self left-tokens right-token)
        do (let ((branch-key (make-branch-key left-tokens branch-index)))
             (setf (gethash branch-key (or-node-branch-successes self)) t)
             (setf (gethash branch-key (or-node-branch-tokens self))
                   (combine-tokens left-tokens right-token))
             (pass-tokens-to-successor 
              self (gethash branch-key (or-node-branch-tokens self))))))

(defmethod accept-tokens-from-left ((self or-node) (left-tokens remove-token))
  (loop for branch-index from 0
        for branch-key = (make-branch-key left-tokens branch-index)
        when (gethash branch-key (or-node-branch-successes self))
        do (progn
             (remhash branch-key (or-node-branch-successes self))
             (let ((stored-token (gethash branch-key (or-node-branch-tokens self))))
               (remhash branch-key (or-node-branch-tokens self))

(defmethod accept-token-from-right ((self or-node) (right-token remove-token))
  (loop for branch-index from 0
        for branch-key = (make-branch-key right-token branch-index)
        when (gethash branch-key (or-node-branch-successes self))
        do (progn
             (remhash branch-key (or-node-branch-successes self))
             (let ((stored-token (gethash branch-key (or-node-branch-tokens self))))
               (remhash branch-key (or-node-branch-tokens self))
               ;; Only propagate removal if no other branches are successful
               (when (zerop (hash-table-count (or-node-branch-successes self)))
                 (when (remove-token-from-right-memory self right-token)
                   (pass-tokens-to-successor 
                    self (make-remove-token stored-token))))))))               
