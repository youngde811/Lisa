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

(in-package :lisa-bridge)

(defvar *bridge-port* 8090
  "Port on which the Lisa bridge HTTP server listens.")

(defvar *acceptor* nil
  "The active Hunchentoot acceptor, or NIL if the server is not running.")

(defun start (&key (port *bridge-port*))
  "Start the Lisa bridge HTTP server on PORT."
  (when *acceptor*
    (error "Lisa bridge is already running on port ~D." (hunchentoot:acceptor-port *acceptor*)))
  (setf *acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port
                       :name 'lisa-bridge))
  (hunchentoot:start *acceptor*)
  (format t "Lisa bridge started on port ~D.~%" port)
  *acceptor*)

(defun stop ()
  "Stop the Lisa bridge HTTP server."
  (unless *acceptor*
    (error "Lisa bridge is not running."))
  (hunchentoot:stop *acceptor*)
  (let ((port (hunchentoot:acceptor-port *acceptor*)))
    (setf *acceptor* nil)
    (format t "Lisa bridge stopped (was on port ~D).~%" port))
  t)
