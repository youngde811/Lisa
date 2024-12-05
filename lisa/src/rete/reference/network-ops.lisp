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

;;; File: network-ops.lisp
;;; Description:

;;; $Id: network-ops.lisp,v 1.22 2007/09/11 21:14:10 youngde Exp $

(in-package "LISA")

(defun add-token-to-network (rete-network token-ctor)
  (loop for root-node being the hash-values of (rete-roots rete-network)
      do (accept-token root-node (funcall token-ctor))))

(defun add-fact-to-network (rete-network fact)
  (add-token-to-network
   rete-network #'(lambda () (make-add-token fact))))

(defun remove-fact-from-network (rete-network fact)
  (add-token-to-network
   rete-network #'(lambda () (make-remove-token fact))))

(defun reset-network (rete-network)
  (add-token-to-network
   rete-network #'(lambda () (make-reset-token t))))

(defmethod decrement-use-count ((node join-node)) 0)
(defmethod decrement-use-count ((node terminal-node)) 0)

(defun remove-rule-from-network (rete-network rule)
  (labels ((remove-nodes (nodes)
             (if (endp nodes) rule
               (let ((node (node-pair-child (first nodes)))
                     (parent (node-pair-parent (first nodes))))
                 (when (zerop (decrement-use-count node))
                   (remove-node-from-parent rete-network parent node))
                 (remove-nodes (rest nodes))))))
    (remove-nodes (rule-node-list rule))))

(defmethod find-existing-successor ((parent shared-node) (node node1))
  (gethash (node1-test node) (shared-node-successors parent)))

(defmethod find-existing-successor (parent node)
  (declare (ignore parent node))
  nil)

(defvar *node-set* nil)

(defmethod add-node-set ((parent shared-node) node &optional (count-p nil))
  (when count-p
    (increment-use-count parent))
  (push (make-node-pair node parent) *node-set*))

(defmethod add-node-set ((parent join-node) node &optional count-p)
  (declare (ignore node count-p))
  nil)

(defmethod add-node-set (parent node &optional count-p)
  (declare (ignore count-p))
  (push (make-node-pair node parent) *node-set*))

(defun merge-networks (from-rete to-rete)
  (labels ((find-root-node (network node)
             (gethash (node1-test node) (rete-roots network)))
           (collect-node-sets (parent children)
             (if (endp children) parent
               (let ((child (first children)))
                 (add-node-set parent child)
                 (when (typep child 'shared-node)
                   (collect-node-sets child 
                                      (shared-node-successor-nodes child)))
                 (collect-node-sets parent (rest children)))))
           (add-new-root (network root)
             (setf (gethash (node1-test root) (rete-roots network)) root)
             (add-node-set t root)
             (collect-node-sets root (shared-node-successor-nodes root)))
           (merge-successors (parent successors)
             (if (endp successors) parent
               (let* ((new-successor (first successors))
                      (existing-successor 
                       (find-existing-successor 
                        parent (successor-node new-successor))))
                 (cond ((null existing-successor)
                        (add-successor parent (successor-node new-successor)
                                       (successor-connector new-successor))
                        (add-node-set parent (successor-node new-successor)))
                       (t
                        (add-node-set 
                         parent (successor-node existing-successor) t)
                        (merge-successors 
                         (successor-node existing-successor)
                         (shared-node-all-successors 
                          (successor-node new-successor)))))
                 (merge-successors parent (rest successors)))))
           (merge-root-node (new-root)
             (let ((existing-root
                    (find-root-node to-rete new-root)))
               (cond ((null existing-root)
                      (add-new-root to-rete new-root))
                     (t
                      (add-node-set t existing-root)
                      (merge-successors
                       existing-root 
                       (shared-node-all-successors new-root)))))))
    (let ((*node-set* (list)))
      (loop for new-root being the hash-values of (rete-roots from-rete)
          do (merge-root-node new-root))
      (nreverse *node-set*))))
