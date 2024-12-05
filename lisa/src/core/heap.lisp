;;; -*- Lisp -*-
;;;
;;; $Header: /cvsroot/lisa/lisa/src/core/heap.lisp,v 1.3 2007/09/11 21:14:09 youngde Exp $
;;;
;;; Copyright (c) 2002, 2003 Gene Michael Stover.
;;; 
;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of version 2.1 of the GNU
;;; Lesser General Public License as published by the Free
;;; Software Foundation.
;;; 
;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;; 
;;; You should have received a copy of the GNU General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA
;;;

;;; Adapted for Lisa: 4/3/2006.

(in-package :lisa.heap)

(defstruct heap
  less-fn
  order
  a
  max-count)

(defun default-search-predicate (heap obj)
  (declare (ignore heap) (ignore obj))
  t)

(defun percolate-down (heap hole x)
  "Private. Move the HOLE down until it's in a location suitable for X.
Return the new index of the hole."
  (do ((a (heap-a heap))
       (less (heap-less-fn heap))
       (child (lesser-child heap hole) (lesser-child heap hole)))
      ((or (>= child (fill-pointer a)) (funcall less x (aref a child)))
       hole)
    (setf (aref a hole) (aref a child)
      hole child)))

(defun percolate-up (heap hole x)
  "Private.  Moves the HOLE until it's in a location suitable for holding
X.  Does not actually bind X to the HOLE.  Returns the new
index of the HOLE.  The hole itself percolates down; it's the X
that percolates up."
  (let ((d (heap-order heap))
	(a (heap-a heap))
	(less (heap-less-fn heap)))
    (setf (aref a 0) x)
    (do ((i hole parent)
	 (parent (floor (/ hole d)) (floor (/ parent d))))
        ((not (funcall less x (aref a parent))) i)
      (setf (aref a i) (aref a parent)))))

(defvar *heap* nil)

(defun heap-init (heap less-fn &key (order 2) (initial-contents nil))
  "Initialize the indicated heap.  If INITIAL-CONTENTS is a non-empty
list, the heap's contents are intiailized to the values in that
list; they are ordered according to LESS-FN.  INITIAL-CONTENTS must
be a list or NIL."
  (setf *heap* heap)
  (setf (heap-less-fn heap) less-fn
        (heap-order heap)   order
        (heap-a heap)       (make-array 2 :initial-element nil
                                        :adjustable t :fill-pointer 1)
        (heap-max-count heap)  0)
  (when initial-contents
    (dolist (i initial-contents) (vector-push-extend i (heap-a heap)))
    (loop for i from (floor (/ (length (heap-a heap)) order)) downto 1
	  do (let* ((tmp (aref (heap-a heap) i))
		    (hole (percolate-down heap i tmp)))
	       (setf (aref (heap-a heap) hole) tmp)))
    (setf (heap-max-count heap) (length (heap-a heap))))
  heap)

(defun create-heap (less-fn &key (order 2) (initial-contents nil))
  (heap-init (make-heap) less-fn :order order
	     :initial-contents initial-contents))

(defun heap-clear (heap)
  "Remove all elements from the heap, leaving it empty.  Faster
(& more convenient) than calling HEAP-REMOVE until the heap is
empty."
  (setf (fill-pointer (heap-a heap)) 1)
  nil)

(defun heap-count (heap)
  (1- (fill-pointer (heap-a heap))))

(defun heap-empty-p (heap)
  "Returns non-NIL if & only if the heap contains no items."
  (= (fill-pointer (heap-a heap)) 1))

(defun heap-insert (heap x)
  "Insert a new element into the heap.  Return the element (which probably
                                                                  isn't very useful)."
  (let ((a (heap-a heap)))
    ;; Append a hole for the new element.
    (vector-push-extend nil a)

    ;; Move the hole from the end towards the front of the
    ;; queue until it is in the right position for the new
    ;; element.
    (setf (aref a (percolate-up heap (1- (fill-pointer a)) x)) x)))

(defun heap-find-idx (heap fnp)
  "Return the index of the element which satisfies the predicate FNP.
If there is no such element, return the fill pointer of HEAP's array A."
  (do* ((a (heap-a heap))
	(fp (fill-pointer a))
	(i  1  (1+ i)))
       ((or (>= i fp) (funcall fnp heap (aref a i)))
	i)))

(defun heap-remove (heap &optional (fn #'default-search-predicate))
  "Remove the minimum (first) element in the heap & return it.  It's
an error if the heap is already empty.  (Should that be an error?)"
  (let ((a (heap-a heap))
	(i (heap-find-idx heap fn)))
    (cond ((< i (fill-pointer a));; We found an element to remove.
           (let ((x (aref a i))
                 (last-object (vector-pop a)))
             (setf (aref a (percolate-down heap i last-object)) last-object)
             x))
          (t nil))));; Nothing to remove

(defun heap-find (heap &optional (fn #'default-search-predicate))
  (let ((a (heap-a heap))
	(i (heap-find-idx heap fn)))
    (cond ((< i (fill-pointer a)) ; We found an element to remove.
           (aref a i))
          (t nil))))

(defun heap-collect (heap &optional (fn #'default-search-predicate))
  (if (heap-empty-p heap)
      nil
    (loop for obj across (heap-a heap)
          when (funcall fn heap obj)
            collect obj)))

(defun heap-peek (heap)
  "Return the first element in the heap, but don't remove it.  It'll
be an error if the heap is empty.  (Should that be an error?)"
  (aref (heap-a heap) 1))

(defun lesser-child (heap parent)
  "Return the index of the lesser child.  If there's one child,
return its index.  If there are no children, return 
(FILL-POINTER (HEAP-A HEAP))."
  (let* ((a (heap-a heap))
         (left (* parent (heap-order heap)))
         (right (1+ left))
         (fp (fill-pointer a)))
    (cond ((>= left fp) fp)
          ((= right fp) left)
          ((funcall (heap-less-fn heap) (aref a left) (aref a right)) left)
          (t right))))

(provide "heap")

;;; --- end of file ---

