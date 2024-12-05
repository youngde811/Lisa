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

;;; File: rpc.lisp
;;; Description:

;;; $Id: rpc.lisp,v 1.5 2002/12/11 19:02:05 youngde Exp $

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'aclrpc)
  (unless (find-package "RPC")
    (defpackage "RPC"
      (:use "COMMON-LISP" "NET.RPC"))))

(in-package "RPC")

(defvar *server-host* "localhost")
(defvar *server-port* 10000)
(defvar *server-proc* nil)

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name)
   (age :initform 0
        :accessor frodo-age)))

(defmethod print-object ((self frodo) strm)
  (print-unreadable-object (strm strm :type t :identity t)
    (format strm "~S, ~S" (frodo-name self) (frodo-age self))))

(defclass remote-frodo (rpc-remote-ref) ())

(defmethod frodo-name ((self remote-frodo))
  (rcall 'frodo-name self))

(defmethod (setf slot-value-of-instance) 
    (new-value (instance rpc-remote-ref) slot-name)
  (rcall 'set-instance-slot instance slot-name new-value))
  
(defun assert-instance (object)
  (format t "instance is ~S~%" object)
  (format t "class-of instance is ~S~%" (class-of object))
  (setf (slot-value-of-instance object 'age) 100)
  object)

(defun initialize-client-environment (port)
  (format t "Initialising client environment~%")
  (import-remote-class port 'remote-frodo "frodo"))

(defun set-instance-slot (object slot value)
  (setf (slot-value object slot) value))

(defun make-server ()
  (make-rpc-server
   'rpc-socket-server
   :name "RPC Server"
   :local-port *server-port*
   :open :listener
   :connect-action :call
   :connect-function
   #'(lambda (port &rest args)
       (initialize-client-environment port)
       (values))))

(defun start-server ()
  (when (null *server-proc*)
    (setf *server-proc* (make-server)))
  *server-proc*)

(defun stop-server ()
  (unless (null *server-proc*)
    (rpc-close :stop :final)
    (setf *server-proc* nil)))

(defun make-client ()
  (make-rpc-client
   'rpc-socket-port
   :remote-host *server-host*
   :remote-port *server-port*))
   
(defun run-client ()
  (multiple-value-bind (port stuff)
      (make-client)
    (with-remote-port (port :close t)
      (let ((frodo (make-instance 'frodo :name 'frodo)))
        (rcall 'assert-instance frodo)
        (format t "frodo instance after remote call: ~S~%" frodo)
        frodo))))

(defun run-sample ()
  (start-server)
  (run-client))
