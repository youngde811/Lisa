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

;;; File: aclrpc-support.lisp
;;; Description: Experimental support for remote object reasoning, using
;;; Allegro's RPC implementation.

;;; $Id: aclrpc-support.lisp,v 1.2 2002/12/12 20:59:18 youngde Exp $

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
