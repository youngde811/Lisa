;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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

;;; File: cmu-auto-notify.lisp
;;; Description: CMU-Lisp-specific implementation of LISA's auto-notification
;;; mechanism, whereby changes to the slot values of CLOS instances, outside
;;; of LISA's control, are picked up via the MOP protocol and synchronized
;;; with KB facts.

;;; cmu-auto-notify.lisp, derived from
;;; $Id: cmucl-auto-notify.lisp,v 1.1 2005/07/30 22:36:33 youngde Exp $

;;; This file courtesy of Fred Gilham.

(in-package "LISA")

(defclass standard-kb-class (standard-class) ())

(defmethod make-instance :around ((self standard-kb-class) 
                                  &rest initargs)
  (declare (ignore initargs))
  (let ((*ignore-this-instance* self))
    (call-next-method)))

(defmethod (setf mop:slot-value-using-class) :after
           (new-value (class standard-kb-class) instance slot)
  (declare (ignore new-value))
  (flet ((ignore-instance (object)
           (and (boundp '*ignore-this-instance*)
                (eq object *ignore-this-instance*))))
    (unless (ignore-instance class)
      (mark-instance-as-changed 
       instance :slot-id (mop:slot-definition-name slot)))))

(defmethod mop:validate-superclass ((class standard-kb-class)
                                (superclass standard-class))
  t)

(eval-when (:load-toplevel)
  (pushnew :lisa-autonotify *features*))
