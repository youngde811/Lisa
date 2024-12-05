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

;;; File: compose.lisp
;;; Description: Utilities used to compose anonymous functions.

;;; $Id: compose.lisp,v 1.5 2001/03/15 16:00:31 youngde Exp $

(in-package "LISA")

(defun build-lambda-expression (forms)
  (labels ((compose-body (forms &optional (body nil))
             (if (null forms)
                 body
               (compose-body (rest forms)
                             (nconc body
                                    `(,(first forms)))))))
    `(lambda ()
       (progn ,@(compose-body forms)))))
  
(defmacro compile-function (forms)
  "Build and compile an anonymous function, using the body provided in
  FORMS."
  `(compile nil (build-lambda-expression ,forms)))
