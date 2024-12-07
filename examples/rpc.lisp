;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.

;; MIT License

;; Copyright (c) 2000 David Young

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

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
