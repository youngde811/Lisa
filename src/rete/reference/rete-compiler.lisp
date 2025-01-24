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

(defvar *root-nodes* nil)
(defvar *rule-specific-nodes* nil)
(defvar *leaf-nodes* nil)
(defvar *logical-block-marker*)

(declaim (inline set-leaf-node leaf-node left-input right-input logical-block-marker))

(defun set-leaf-node (node address)
  (setf (aref *leaf-nodes* address) node))

(defun leaf-node ()
  (aref *leaf-nodes* (1- (length *leaf-nodes*))))

(defun left-input (address)
  (aref *leaf-nodes* (1- address)))

(defun right-input (address)
  (aref *leaf-nodes* address))

(defun logical-block-marker ()
  *logical-block-marker*)
  
(defclass rete-network ()
  ((root-nodes :initform (make-hash-table)
               :initarg :root-nodes
               :reader rete-roots)
   (node-test-cache :initform (make-hash-table :test #'equal)
                    :initarg :node-test-cache
                    :reader node-test-cache)))

(defun record-node (node parent)
  (when (typep parent 'shared-node)
    (increment-use-count parent))
  (push (make-node-pair node parent) *rule-specific-nodes*)
  node)

(defmethod remove-node-from-parent ((self rete-network) (parent t) child)
  (remhash (node1-test child) (rete-roots self)))

(defmethod remove-node-from-parent ((self rete-network) 
                                    (parent shared-node) child)
  (remove-successor parent child))

(defun make-root-node (class)
  (let* ((test (make-class-test class))
         (root (gethash test *root-nodes*)))
    (when (null root)
      (setf root (make-node1 test))
      (setf (gethash test *root-nodes*) root))
    (record-node root t)))

(defmethod add-successor ((parent t) new-node connector)
  (declare (ignore connector))
  new-node)

(defmethod add-successor :around ((parent shared-node) new-node connector)
  (declare (ignore new-node connector))
  (record-node (call-next-method) parent))

(defun make-intra-pattern-node (slot)
  (let ((test
         (cond ((simple-slot-p slot)
                (make-simple-slot-test slot))
               ((constrained-slot-p slot)
                (make-intra-pattern-constraint-test slot))
               (t
                (make-intra-pattern-test slot)))))
    (make-node1 test)))

(defun distribute-token (rete-network token)
  (loop for root-node being the hash-values 
      of (rete-roots rete-network)
      do (accept-token root-node token)))

(defmethod make-rete-network (&rest args &key &allow-other-keys)
  (apply #'make-instance 'rete-network args))

;;; The following functions serve as "connectors" between any two
;;; nodes. PASS-TOKEN connects two pattern (one-input) nodes, or a join node 
;;; to a terminal node; ENTER-JOIN-NETWORK-FROM-LEFT connects a pattern node
;;; to a join node; ENTER-JOIN-NETWORK-FROM-RIGHT also connects a pattern node
;;; to a join node; both PASS-TOKENS-ON-LEFT and PASS-TOKEN-ON-RIGHT connect
;;; two join nodes.

(defun pass-token (node token)
  (accept-token node token))

(defun pass-tokens-on-left (node2 tokens)
  (accept-tokens-from-left node2 tokens))

(defun pass-token-on-right (node2 token)
  (accept-token-from-right node2 token))

(defun enter-join-network-from-left (node2 tokens)
  (pass-tokens-on-left node2 (replicate-token tokens)))

(defun enter-join-network-from-right (node2 token)
  (pass-token-on-right node2 (replicate-token token)))

;;; end connector functions

(defun add-intra-pattern-nodes (patterns)
  "The alpha memory nodes and tests"
  (dolist (pattern patterns)
    (cond ((test-pattern-p pattern)
           (set-leaf-node t (parsed-pattern-address pattern)))
          (t
           (let ((node
                  (make-root-node (parsed-pattern-class pattern)))
                 (address (parsed-pattern-address pattern)))
             (set-leaf-node node address)
             (dolist (slot (parsed-pattern-slots pattern))
               (when (intra-pattern-slot-p slot)
                 (setf node
                   (add-successor node (make-intra-pattern-node slot)
                                  #'pass-token))
                 (set-leaf-node node address))))))))

(defun add-join-node-tests (join-node pattern)
  (labels ((add-simple-join-node-test (slot)
             (unless (= (binding-address (pattern-slot-slot-binding slot))
                        (parsed-pattern-address pattern))
               (join-node-add-test join-node
                                   (make-inter-pattern-test slot))))
           (add-slot-constraint-test (slot)
             (join-node-add-test join-node
                                 (make-predicate-test
                                  (pattern-slot-constraint slot)
                                  (pattern-slot-constraint-bindings slot)
                                  (negated-slot-p slot))))
           (add-test-pattern-predicate ()
             (join-node-add-test join-node
                                 (make-predicate-test
                                  (parsed-pattern-test-forms pattern)
                                  (parsed-pattern-test-bindings pattern))))
           (add-generic-pattern-tests ()
             (dolist (slot (parsed-pattern-slots pattern))
               (cond ((simple-bound-slot-p slot)
                      (add-simple-join-node-test slot))
                     ((constrained-slot-p slot)
                      (add-slot-constraint-test slot))))))
    (if (test-pattern-p pattern)
        (add-test-pattern-predicate)
      (add-generic-pattern-tests))
    join-node))

(defun make-join-node (pattern)
  (let ((join-node
         (cond ((negated-pattern-p pattern)
                (make-node2-not))
               ((test-pattern-p pattern)
                (make-node2-test))
               ((existential-pattern-p pattern)
                (make-node2-exists))
               (t (make-node2)))))
    (when (eql (parsed-pattern-address pattern) (logical-block-marker))
      (mark-as-logical-block join-node (logical-block-marker)))
    join-node))

(defun make-left-join-connection (join-node node)
  (if (typep node 'shared-node)
      (add-successor node join-node #'enter-join-network-from-left)
    (add-successor node join-node #'pass-tokens-on-left))
  join-node)

(defun make-right-join-connection (join-node node)
  (if (typep node 'shared-node)
      (add-successor node join-node #'enter-join-network-from-right)
    (add-successor node join-node #'pass-token-on-right))
  join-node)

(defun add-inter-pattern-nodes (patterns)
  "The beta memory nodes and tests"
  (dolist (pattern (rest patterns))
    (let ((join-node (make-join-node pattern))
          (address (parsed-pattern-address pattern)))
      (add-join-node-tests join-node pattern)
      (make-left-join-connection join-node (left-input address))
      (make-right-join-connection join-node (right-input address))
      (set-leaf-node join-node address))))

(defun add-terminal-node (rule)
  (add-successor (leaf-node) (make-terminal-node rule) #'pass-token))

;;; addresses a problem reported by Andrew Philpot on 9/6/2007

(defun copy-node-test-table (src)
  (let ((target (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key target) value))
             src)
    target))

(defun compile-rule-into-network (rete-network patterns rule)
  (let ((*root-nodes* (rete-roots rete-network))
        (*rule-specific-nodes* (list))
        (*leaf-nodes* (make-array (length patterns)))
        (*logical-block-marker* (rule-logical-marker rule))
        (*node-test-table* (node-test-cache rete-network)))
    (add-intra-pattern-nodes patterns)
    (add-inter-pattern-nodes patterns)
    (add-terminal-node rule)
    (attach-rule-nodes rule (nreverse *rule-specific-nodes*))
    (setf (slot-value rete-network 'root-nodes) *root-nodes*)
    rete-network))

(defun merge-rule-into-network (to-network patterns rule &key (loader nil))
  (let ((from-network
         (compile-rule-into-network
          (make-rete-network :node-test-cache (copy-node-test-table (node-test-cache to-network)))
          patterns rule)))
    (when loader
      (funcall loader from-network))
    (attach-rule-nodes rule (merge-networks from-network to-network))
    to-network))
