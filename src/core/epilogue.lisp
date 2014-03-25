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

;;; File: epilogue.lisp
;;; Description:

;;; $Id: epilogue.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package "LISA")

(deftemplate initial-fact ())

(deftemplate query-fact ())

;;; This macro is courtesy of Paul Werkowski. A very nice idea.

(defmacro define-lisa-lisp ()
  (flet ((externals-of (pkg)
           (loop for s being each external-symbol in pkg collect s)))
    (let* ((lisa-externs (externals-of "LISA"))
           (lisa-shadows (intersection (package-shadowing-symbols "LISA")
                                       lisa-externs))
           (cl-externs (externals-of "COMMON-LISP")))
      `(defpackage "LISA-LISP"
         (:use "COMMON-LISP")
         (:shadowing-import-from "LISA" ,@lisa-shadows)
         (:import-from "LISA" ,@(set-difference lisa-externs lisa-shadows))
         (:export ,@cl-externs)
         (:export ,@lisa-externs)))))

(eval-when (:load-toplevel :execute)
  (make-default-inference-engine)
  (setf *active-context* (initial-context (inference-engine)))
  (define-lisa-lisp)
  (when (use-fancy-assert)
    (set-dispatch-macro-character
     #\# #\? #'(lambda (strm subchar arg)
                 (declare (ignore subchar arg))
                 (list 'identity (read strm t nil t)))))
  (pushnew :lisa *features*))


