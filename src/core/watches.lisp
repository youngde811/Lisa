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

;;; File: watches.lisp
;;; Description:

;;; $Id: watches.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package "LISA")

(defvar *assert-fact* nil)
(defvar *retract-fact* nil)
(defvar *enable-activation* nil)
(defvar *disable-activation* nil)
(defvar *fire-rule* nil)
(defvar *watches* nil)

(defun watch-activation-detail (activation direction)
  (format *trace-output* "~A Activation: ~A : ~A~%"
          direction
          (rule-default-name (activation-rule activation))
          (activation-fact-list activation))
  (values))

(defun watch-enable-activation (activation)
  (watch-activation-detail activation "==>"))

(defun watch-disable-activation (activation)
  (watch-activation-detail activation "<=="))

(defun watch-rule-firing (activation)
  (let ((rule (activation-rule activation)))
    (format *trace-output* "FIRE ~D: ~A ~A~%"
            (rete-firing-count (rule-engine rule))
            (rule-default-name rule)
            (activation-fact-list activation))
    (values)))

(defun watch-fact-detail (fact direction)
  (format *trace-output* "~A ~A ~S~%"
          direction (fact-symbolic-id fact)
          (reconstruct-fact fact))
  (values))

(defun watch-assert (fact)
  (watch-fact-detail fact "==>"))

(defun watch-retract (fact)
  (watch-fact-detail fact "<=="))

(defun watch-event (event)
  (ecase event
    (:facts (setf *assert-fact* #'watch-assert)
            (setf *retract-fact* #'watch-retract))
    (:activations (setf *enable-activation* #'watch-enable-activation)
                  (setf *disable-activation* #'watch-disable-activation))
    (:rules (setf *fire-rule* #'watch-rule-firing))
    (:all (watch-event :facts)
          (watch-event :activations)
          (watch-event :rules)))
  (unless (eq event :all)
    (pushnew event *watches*))
  event)

(defun unwatch-event (event)
  (ecase event
    (:facts (setf *assert-fact* nil)
            (setf *retract-fact* nil))
    (:activations (setf *enable-activation* nil)
                  (setf *disable-activation* nil))
    (:rules (setf *fire-rule* nil))
    (:all (unwatch-event :facts)
          (unwatch-event :activations)
          (unwatch-event :rules)))
  (unless (eq event :all)
    (setf *watches*
      (delete event *watches*)))
  event)

(defun watches ()
  *watches*)

(defmacro trace-assert (fact)
  `(unless (null *assert-fact*)
     (funcall *assert-fact* ,fact)))

(defmacro trace-retract (fact)
  `(unless (null *retract-fact*)
     (funcall *retract-fact* ,fact)))

(defmacro trace-enable-activation (activation)
  `(unless (null *enable-activation*)
     (funcall *enable-activation* ,activation)))

(defmacro trace-disable-activation (activation)
  `(unless (null *disable-activation*)
     (funcall *disable-activation* ,activation)))

(defmacro trace-firing (activation)
  `(unless (null *fire-rule*)
     (funcall *fire-rule* ,activation)))
