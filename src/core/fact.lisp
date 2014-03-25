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

;;; File: fact.lisp
;;; Description:

;;; $Id: fact.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package :lisa)

(defclass fact ()
  ((name :initarg :name
         :reader fact-name)
   (id :initform -1
       :accessor fact-id)
   (slot-table :reader fact-slot-table
               :initform (make-hash-table :test #'equal))
   (belief :initarg :belief
           :initform nil
           :accessor belief-factor)
   (clos-instance :reader fact-clos-instance)
   (shadows :initform nil
            :reader fact-shadowsp)
   (meta-data :reader fact-meta-data))
  (:documentation
   "This class represents all facts in the knowledge base."))

(defmethod equals ((fact-1 fact) (fact-2 fact))
  (and (eq (fact-name fact-1) (fact-name fact-2))
       (equalp (fact-slot-table fact-1) (fact-slot-table fact-2))))

(defmethod hash-key ((self fact))
  (let ((key (list)))
    (maphash #'(lambda (slot value)
                 (declare (ignore slot))
                 (push value key))
             (fact-slot-table self))
    (push (fact-name self) key)
    key))

(defmethod slot-value-of-instance ((object t) slot-name)
  (slot-value object slot-name))

(defmethod (setf slot-value-of-instance) (new-value (object t) slot-name)
  (setf (slot-value object slot-name) new-value))

(defun fact-symbolic-id (fact)
  (format nil "F-~D" (fact-id fact)))

(defun set-slot-value (fact slot-name value)
  "Assigns a new value to a slot in a fact and its associated CLOS
  instance. SLOT-NAME is a symbol; VALUE is the new value for the
  slot."
  (with-auto-notify (object (find-instance-of-fact fact))
    (setf (slot-value-of-instance object slot-name) value)
    (initialize-slot-value fact slot-name value)))

(defun initialize-slot-value (fact slot-name value)
  "Sets the value of a slot in a fact's slot table. FACT is a FACT instance;
  SLOT-NAME is a symbol; VALUE is the slot's new value."
  (setf (gethash slot-name (fact-slot-table fact)) value)
  fact)

(defun set-slot-from-instance (fact instance slot-name)
  "Assigns to a slot the value from the corresponding slot in the fact's CLOS
  instance. FACT is a FACT instance; META-FACT is a META-FACT instance;
  INSTANCE is the fact's CLOS instance; SLOT-NAME is a symbol representing the
  affected slot."
  (initialize-slot-value
   fact slot-name
   (slot-value-of-instance instance slot-name)))

(defun get-slot-values (fact)
  "Returns a list of slot name / value pairs for every slot in a fact. FACT is
  a fact instance."
  (let ((slots (list)))
    (maphash #'(lambda (slot value)
                 (push (list slot value) slots))
             (fact-slot-table fact))
    slots))

(defmethod get-slot-value ((self fact) (slot-name symbol))
  "Returns the value associated with a slot name. FACT is a FACT instance;
  SLOT-NAME is a SLOT-NAME instance."
  (gethash slot-name (fact-slot-table self)))

(defmethod get-slot-value ((self fact) (slot-name (eql :object)))
  (fact-clos-instance self))

(defun find-instance-of-fact (fact)
  "Retrieves the CLOS instance associated with a fact. FACT is a FACT
  instance."
  (fact-clos-instance fact))

;;; Corrected version courtesy of Aneil Mallavarapu...

(defun has-superclass (fact symbolic-name) ; fix converts symbolic-name to a class-object
  (find (find-class symbolic-name) (get-superclasses (fact-meta-data fact))))

(defun synchronize-with-instance (fact &optional (effective-slot nil))
  "Makes a fact's slot values and its CLOS instance's slot values match. If a
  slot identifier is provided then only that slot is synchronized. FACT
  is a FACT instance; EFFECTIVE-SLOT, if supplied, is a symbol representing
  the CLOS instance's slot."
  (let ((instance (find-instance-of-fact fact))
        (meta (fact-meta-data fact)))
    (flet ((synchronize-all-slots ()
             (mapc #'(lambda (slot-name)
                       (set-slot-from-instance fact instance slot-name))
                   (get-slot-list meta)))
           (synchronize-this-slot ()
             (set-slot-from-instance fact instance effective-slot)))
      (if (null effective-slot)
          (synchronize-all-slots)
        (synchronize-this-slot)))
    fact))

(defun reconstruct-fact (fact)
  `(,(fact-name fact) ,@(get-slot-values fact)))

(defmethod print-object ((self fact) strm)
  (print-unreadable-object (self strm :type nil :identity t)
    (format strm "~A ; id ~D" (fact-name self) (fact-id self))))

(defmethod initialize-instance :after ((self fact) &key (slots nil)
                                                        (instance nil))
  "Initializes a FACT instance. SLOTS is a list of slot name / value pairs,
  where (FIRST SLOTS) is a symbol and (SECOND SLOT) is the slot's
  value. INSTANCE is the CLOS instance to be associated with this FACT; if
  INSTANCE is NIL then FACT is associated with a template and a suitable
  instance must be created; otherwise FACT is bound to a user-defined class."
  (with-slots ((slot-table slot-table)
               (meta-data meta-data)) self
    (setf meta-data (find-meta-fact (fact-name self)))
    (mapc #'(lambda (slot-name)
              (setf (gethash slot-name slot-table) nil))
          (get-slot-list meta-data))
    (if (null instance)
        (initialize-fact-from-template self slots meta-data)
      (initialize-fact-from-instance self instance meta-data))
    self))

(defun initialize-fact-from-template (fact slots meta-data)
  "Initializes a template-bound FACT. An instance of the FACT's associated
  class is created and the slots of both are synchronized from the SLOTS
  list. FACT is a FACT instance; SLOTS is a list of symbol/value pairs."
  (let ((instance
         (make-instance (find-class (get-class-name meta-data) nil))))
    (cl:assert (not (null instance)) nil
      "No class was found corresponding to fact name ~S." (fact-name fact))
    (setf (slot-value fact 'clos-instance) instance)
    (mapc #'(lambda (slot-spec)
              (let ((slot-name (first slot-spec))
                    (slot-value (second slot-spec)))
                (set-slot-value fact slot-name slot-value)))
          slots)
    fact))

(defun initialize-fact-from-instance (fact instance meta-data)
  "Initializes a fact associated with a user-created CLOS instance. The fact's
  slot values are taken from the CLOS instance. FACT is a FACT instance;
  INSTANCE is the CLOS instance associated with this fact."
  (mapc #'(lambda (slot-name)
            (set-slot-from-instance fact instance slot-name))
        (get-slot-list meta-data))
  (setf (slot-value fact 'clos-instance) instance)
  (setf (slot-value fact 'shadows) t)
  fact)

(defun make-fact (name &rest slots)
  "The default constructor for class FACT. NAME is the symbolic fact name as
  used in rules; SLOTS is a list of symbol/value pairs."
  (make-instance 'fact :name name :slots slots))

(defun make-fact-from-instance (name clos-instance)
  "A constructor for class FACT that creates an instance bound to a
  user-defined CLOS instance. NAME is the symbolic fact name; CLOS-INSTANCE is
  a user-supplied CLOS object."
  (make-instance 'fact :name name :instance clos-instance))
  
(defun make-fact-from-template (fact)
  "Creates a FACT instance using another FACT instance as a
  template. Basically a clone operation useful for such things as asserting
  DEFFACTS."
  (apply #'make-fact
         (fact-name fact)
         (mapcar #'(lambda (slot-name)
                     (list slot-name (get-slot-value fact slot-name)))
                 (get-slot-list (fact-meta-data fact)))))
