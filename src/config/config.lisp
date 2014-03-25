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

;;; File: config.lisp
;;; Description: User-customisable configuration settings for LISA. It is
;;; expected that developers will edit this file as they see fit.

;;; $Id: config.lisp,v 1.1 2002/12/04 14:41:55 youngde Exp $

(in-package "LISA")

;;; The reference guide has complete details, but:
;;;
;;; * Setting USE-FANCY-ASSERT enables the #? dispatch macro character.
;;; * Setting ALLOW-DUPLICATE-FACTS disables duplicate fact checking during
;;;   assertions.
;;; * Setting CONSIDER-TAXONOMY instructs LISA to consider a CLOS instance's
;;;   ancestors during pattern matching.

(eval-when (:load-toplevel)
  (setf (use-fancy-assert) t)
  (setf (allow-duplicate-facts) t)
  (setf (consider-taxonomy) t))
