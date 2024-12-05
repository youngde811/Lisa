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

;;; File: node2-test.lisp
;;; Description:

;;; $Id: node2-test.lisp,v 1.6 2007/09/11 21:14:10 youngde Exp $

(in-package "LISA")

(defclass node2-test (join-node) ())

(defmethod accept-tokens-from-left ((self node2-test) (left-tokens add-token))
  (add-tokens-to-left-memory self left-tokens)
  (when (every #'(lambda (test)
                   (funcall test left-tokens))
               (join-node-tests self))
    (pass-tokens-to-successor self (combine-tokens left-tokens self))))

(defmethod accept-tokens-from-left ((self node2-test) (left-tokens remove-token))
  (when (remove-tokens-from-left-memory self left-tokens)
    (pass-tokens-to-successor self (combine-tokens left-tokens self))))

(defun make-node2-test ()
  (make-instance 'node2-test))
