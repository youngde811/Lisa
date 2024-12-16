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

(defclass shared-node ()
  ((successors :initform (make-hash-table :test #'equal)
               :reader shared-node-successors)
   (refcnt :initform 0
           :accessor shared-node-refcnt)))

(defmethod increment-use-count ((self shared-node))
  (incf (shared-node-refcnt self)))

(defmethod decrement-use-count ((self shared-node))
  (decf (shared-node-refcnt self)))

(defmethod node-use-count ((self shared-node))
  (shared-node-refcnt self))

(defmethod node-referenced-p ((self shared-node))
  (plusp (node-use-count self)))

(defmethod pass-token-to-successors ((self shared-node) token)
  (loop for successor being the hash-values of (shared-node-successors self)
      do (funcall (successor-connector successor)
                  (successor-node successor)
                  token)))

(defun shared-node-successor-nodes (shared-node)
  (loop for successor being the hash-values of (shared-node-successors shared-node)
      collect (successor-node successor)))

(defun shared-node-all-successors (shared-node)
  (loop for successor being the hash-values of (shared-node-successors shared-node)
      collect successor))
