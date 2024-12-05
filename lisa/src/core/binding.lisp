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

;;; File: binding.lisp
;;; Description:

;;; $Id: binding.lisp,v 1.3 2007/09/11 21:14:09 youngde Exp $

(in-package :lisa)

(defstruct (binding
            (:type list)
            (:constructor %make-binding))
  variable address slot-name)

(defun make-binding (var address slot-name)
  (%make-binding :variable var :address address :slot-name slot-name))

(defun pattern-binding-p (binding)
  (eq (binding-slot-name binding) :pattern))
