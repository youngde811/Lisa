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

;;; File: environment.lisp
;;; Description: Defines the standard LISA environment.

;;; $Id: environment.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package "LISA")

(defvar *default-engine* nil
  "The currently-active inference engine.")

(defun use-default-engine ()
  "Create and make available a default instance of the inference engine. Use
    this function when you want a basic, single-threaded LISA environment."
  (when (null *default-engine*)
    (setf *default-engine* (make-inference-engine)))
  (values *default-engine*))

(defun use-engine (engine)
  "Make ENGINE the default inference engine. Use this function with great care
  in an MP environment."
  (setf *default-engine* engine))

(defun current-engine (&optional (errorp t))
  "Returns the currently-active inference engine. Usually only invoked by code
  running within the context of WITH-INFERENCE-ENGINE."
  (when errorp
    (cl:assert (not (null *default-engine*)) (*default-engine*)
               "The current inference engine has not been established."))
  (values *default-engine*))

(defmacro with-inference-engine ((engine) &body body)
  "Evaluates BODY within the context of the inference engine ENGINE. This
    macro is MP-safe."
  `(let ((*default-engine* ,engine))
    (progn ,@body)))

(defun clear-environment (engine)
  "Completely resets the inference engine ENGINE."
  (clear-engine engine)
  (values))
