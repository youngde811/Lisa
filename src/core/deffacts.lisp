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

;;; File: deffacts.lisp
;;; Description: This class represents "autoloaded" facts that are asserted
;;; automatically as part of an engine reset.

;;; $Id: deffacts.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package "LISA")

(defclass deffacts ()
  ((name :initarg :name
         :reader deffacts-name)
   (fact-list :initarg :fact-list
              :initform nil
              :reader deffacts-fact-list))
  (:documentation
   "This class represents 'autoloaded' facts that are asserted automatically
  as part of an inference engine reset."))

(defmethod print-object ((self deffacts) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "~S ; ~S" (deffacts-name self) (deffacts-fact-list self))))

(defun make-deffacts (name facts)
  (make-instance 'deffacts :name name :fact-list (copy-list facts)))

