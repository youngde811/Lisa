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

;;; Description: A sample implementation of an RPC server capable of reasoning
;;; over remote objects.

(in-package "RPC")

(defvar *lisa-server-host* "localhost")
(defvar *lisa-server-port* 10000)
(defvar *server-proc* nil)

(defun assert-object (object)
  (let ((*package* (find-package "RPC")))
    (format t "package is ~S~%" *package*)
    (format t "object is ~S~%" object)
    (format t "class of object is ~S~%" (class-of object))
    (format t "class name of object is ~S~%" (class-name (class-of object)))
    (assert-instance object)
    object))

(defun initialize-client-environment (port)
  (format t "Initialising client environment~%")
  (import-remote-class port 'remote-instance "frodo"))

(defun make-server ()
  (make-rpc-server
   'rpc-socket-server
   :name "Lisa RPC Server"
   :local-port *lisa-server-port*
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

(defrule remote-frodo ()
  (?frodo (frodo (has-ring :no)))
  =>
  (modify ?frodo (has-ring t) (companions '(samwise gandalf))))
