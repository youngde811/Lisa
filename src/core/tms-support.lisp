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

;;; File: tms-support.lisp
;;; Description: Support functions for LISA's Truth Maintenance System (TMS).

;;; $Id: tms-support.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package "LISA")

(defvar *scheduled-dependencies*)

(define-symbol-macro scheduled-dependencies *scheduled-dependencies*)

(defun add-logical-dependency (rete fact dependency-set)
  (setf (gethash dependency-set (rete-dependency-table rete))
    (push fact (gethash dependency-set (rete-dependency-table rete)))))

(defun find-logical-dependencies (rete dependency-set)
  (gethash dependency-set (rete-dependency-table rete)))

(defun make-dependency-set (tokens marker)
  (let ((dependencies (list)))
    (loop for i from 1 to marker
        do (push (token-find-fact tokens i) dependencies))
    (nreverse dependencies)))

(defun schedule-dependency-removal (dependency-set)
  (push dependency-set scheduled-dependencies))

(defmacro with-truth-maintenance ((rete) &body body)
  (let ((rval (gensym)))
    `(let* ((*scheduled-dependencies* (list))
            (,rval
             (progn ,@body)))
       (dolist (dependency scheduled-dependencies)
         (with-accessors ((table rete-dependency-table)) ,rete
           (dolist (dependent-fact
                       (gethash dependency table)
                     (remhash dependency table))
             (retract-fact ,rete dependent-fact))))
       ,rval)))
