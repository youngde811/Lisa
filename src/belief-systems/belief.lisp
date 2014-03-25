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

;;; File: belief.lisp

;;; Description: Common interfaces to Lisa's belief-system interface.

;;; $Id: belief.lisp,v 1.1 2006/04/14 16:41:07 youngde Exp $

(in-package :belief)

;;; The principal interface by which outside code hooks objects that support some kind of belief-factor
;;; interface into this library.

(defgeneric belief-factor (obj))
(defgeneric adjust-belief (objects rule-belief &optional old-belief))
(defgeneric belief->english (belief-factor))
