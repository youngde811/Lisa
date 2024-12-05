;;; This file is part of Lisa, the Lisp-based Intelligent Software
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

;;; File: fact-parser.lisp
;;; Description: 
;;;
;;; $Id: fact-parser.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package :lisa)

(defun create-template-class-slots (class-name slot-list)
  (labels ((determine-default (default-form)
             (unless (and (consp default-form)
                          (eq (first default-form) 'default)
                          (= (length default-form) 2))
               (error 'class-parsing-error :class-name class-name
                      :text "malformed DEFAULT keyword"))
             (second default-form))
           (build-one-slot (template)
             (destructuring-bind (keyword slot-name &optional default)
                 template
               (unless (eq keyword 'slot)
                 (error 'class-parsing-error :class-name class-name
                        :text "unrecognized keyword: ~A" keyword))
               `(,slot-name
                 :initarg ,(intern (symbol-name slot-name) 'keyword)
                 :initform
                 ,(if (null default) nil (determine-default default))
                 :reader 
                 ,(intern (format nil "~S-~S" class-name slot-name))))))
    (mapcar #'build-one-slot slot-list)))

(defun redefine-deftemplate (class-name body)
  (let ((class (gensym)))
    `(let ((,class
            (defclass ,class-name (inference-engine-object)
              ,@(list (create-template-class-slots class-name body)))))
       ,class)))

(defun bind-logical-dependencies (fact)
  (add-logical-dependency 
   (inference-engine) fact 
   (make-dependency-set (active-tokens) (rule-logical-marker (active-rule))))
  fact)
  
(defun parse-and-insert-instance (instance &key (belief nil))
  (ensure-meta-data-exists (class-name (class-of instance)))
  (let ((fact
         (make-fact-from-instance (class-name (class-of instance)) instance)))
    (when (and (in-rule-firing-p)
               (logical-rule-p (active-rule)))
      (bind-logical-dependencies fact))
    (assert-fact (inference-engine) fact :belief belief)))

(defun parse-and-retract-instance (instance engine)
  (retract-fact engine instance))

(defun show-deffacts (deffact)
  (format t "~S~%" deffact)
  (values deffact))

(defun parse-and-insert-deffacts (name body)
  (let ((deffacts (gensym)))
    `(let ((,deffacts (list)))
       (dolist (fact ',body)
         (let ((head (first fact)))
           (ensure-meta-data-exists head)
           (push 
            (apply #'make-fact head (rest fact))
            ,deffacts)))
       (add-autofact (inference-engine) (make-deffacts ',name (nreverse ,deffacts))))))
       
