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

;;; File: conditions.lisp
;;; Description:

;;; $Id: conditions.lisp,v 1.3 2007/09/07 21:32:05 youngde Exp $

(in-package :lisa)

(define-condition duplicate-fact (error)
  ((existing-fact :reader duplicate-fact-existing-fact
                  :initarg :existing-fact))
  (:report (lambda (condition strm)
             (format strm "Lisa detected an attempt to assert a duplicate for: ~S"
                     (duplicate-fact-existing-fact condition)))))
                  
(define-condition parsing-error (error)
  ((text :initarg :text
         :initform nil
         :reader text)
   (location :initarg :location
             :initform nil
             :reader location))
  (:report (lambda (condition strm)
             (format strm "Parsing error: ~A" (text condition)))))

(define-condition slot-parsing-error (parsing-error)
  ((slot-name :initarg :slot-name
              :initform nil
              :reader slot-name))
  (:report (lambda (condition strm)
             (format strm "Slot parsing error: slot ~A, pattern location ~A"
                     (slot-name condition) (location condition))
             (when (text condition)
               (format strm " (~A)" (text condition))))))

(define-condition class-parsing-error (parsing-error)
  ((class-name :initarg :class-name
               :initform nil
               :reader class-name))
  (:report (lambda (condition strm)
             (format strm "Class parsing error: ~A, ~A" (class-name condition) (text condition)))))

(define-condition rule-parsing-error (parsing-error)
  ((rule-name :initarg :rule-name
              :initform nil
              :reader rule-name))
  (:report (lambda (condition strm)
             (format strm "Rule parsing error: rule name ~A, pattern location ~A"
                     (rule-name condition) (location condition))
             (when (text condition)
               (format strm " (~A)" (text condition))))))