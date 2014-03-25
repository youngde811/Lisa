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

;;; File:
;;; Description:

;;; $Id: metaclass.lisp,v 1.11 2002/12/03 16:03:47 youngde Exp $

(in-package "CL-USER")

(defclass standard-kb-class (standard-class) ())

(defmethod initialize-instance :after ((self standard-kb-class) &rest initargs)
  (dolist (slot (slot-value self 'clos::direct-slots))
    (dolist (writer (clos:slot-definition-writers slot))
      (let* ((gf (ensure-generic-function writer))
             (method-class
              (generic-function-method-class gf)))
        (multiple-value-bind (body initargs)
            (clos:make-method-lambda
             gf
             (class-prototype method-class)
             '(new-value object)
             nil
             `(format t "setting slot ~S to ~S~%" ',(clos:slot-definition-name slot) new-value))
          (clos:add-method
           gf
           (apply #'make-instance method-class
                  :function (compile nil body)
                  :specializers
                  `(,(find-class t) ,self)
                  :qualifiers '(:after)
                  :lambda-list '(value object)
                  initargs)))))))

(defmethod validate-superclass ((class standard-kb-class)
                                (superclass standard-class))
  t)

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name)
   (age :initarg :age
        :initform 100
        :accessor frodo-age))
  (:metaclass standard-kb-class))

(defparameter *frodo* (make-instance 'frodo :name 'frodo))
