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

(defclass node1 (shared-node)
  ((test :initarg :test
         :reader node1-test)))

(defmethod add-successor ((self node1) (new-node node1) connector)
  (with-slots ((successor-table successors)) self
    (let ((successor (gethash (node1-test new-node) successor-table)))
      (when (null successor)
        (setf successor
          (setf (gethash (node1-test new-node) successor-table)
            (make-successor new-node connector))))
      (successor-node successor))))

(defmethod add-successor ((self node1) (new-node t) connector)
  (setf (gethash `(,new-node ,connector) (shared-node-successors self))
    (make-successor new-node connector))
  new-node)

(defmethod remove-successor ((self node1) successor-node)
  (let ((successors (shared-node-successors self)))
    (maphash #'(lambda (key successor)
                 (when (eq successor-node (successor-node successor))
                   (remhash key successors)))
             successors)
    successor-node))

(defmethod accept-token ((self node1) token)
  (if (funcall (node1-test self) token)
      (pass-token-to-successors self token)
    nil))

(defmethod accept-token ((self node1) (token reset-token))
  (pass-token-to-successors self (token-push-fact token t)))

(defmethod print-object ((self node1) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "~S ; ~D" (node1-test self) (node-use-count self))))

(defun make-node1 (test)
  (make-instance 'node1 :test test))

