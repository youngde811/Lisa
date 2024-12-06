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

;; Description: A sample implementation of an RPC client that requests
;; inferencing services from a Lisa server.

(in-package "RPC")

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name)
   (has-ring :initform :no
             :accessor frodo-has-ring)
   (companions :initform nil
               :accessor frodo-companions)))

(defun set-slot-value (new-value instance slot-name)
  (setf (slot-value instance slot-name) new-value))

(defmethod print-object ((self frodo) strm)
  (print-unreadable-object (strm strm :type t :identity t)
    (format strm "~S, ~S, ~S" 
            (frodo-name self)
            (frodo-has-ring self)
            (frodo-companions self))))

(defun make-client ()
  (make-rpc-client
   'rpc-socket-port
   :remote-host *lisa-server-host*
   :remote-port *lisa-server-port*))
   
(defun run-client ()
  (let ((frodo (make-instance 'frodo :name 'frodo)))
    (format t "Frodo instance before inferencing: ~S~%" frodo)
    (multiple-value-bind (port stuff)
        (make-client)
      (with-remote-port (port :close t)
        (rcall 'reset)
        (rcall 'assert-object frodo)
        (rcall 'run)
        (format t "Frodo instance after inferencing: ~S~%" frodo)
        frodo))))

(defun run-all ()
  (start-server)
  (run-client))
