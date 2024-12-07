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

;; Description: Experimental support for remote object reasoning, using Allegro's RPC implementation.

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'aclrpc)
  (unless (find-package "LISA.RPC")
    (defpackage "LISA.RPC"
      (:use "LISA-LISP" "NET.RPC")
      (:nicknames "RPC"))))

(in-package "LISA.RPC")

(defclass remote-kb-class (standard-class)
  ((proxy-class-name :reader proxy-class-name)))
  
(defclass remote-instance (rpc-remote-ref)
  ()
  (:metaclass remote-kb-class))

(defmethod initialize-instance :after ((self remote-instance) &rest args)
  (declare (ignore args))
  (setf (slot-value (class-of self) 'proxy-class-name)
    (intern (rr-type self) 'rpc)))

(defmethod class-name ((class remote-kb-class))
  (proxy-class-name class))

(defmethod lisa:slot-value-of-instance ((object remote-instance) slot-name)
  (rcall 'slot-value object slot-name))

(defmethod (setf lisa:slot-value-of-instance) 
    (new-value (object remote-instance) slot-name)
  (rcall 'set-slot-value new-value object slot-name))
