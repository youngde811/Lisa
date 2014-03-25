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

;;; File: reflect.lisp
;;; Description: Wrapper functions that provide the MOP functionality needed
;;; by LISA, hiding implementation-specific details.

;;; $Id: reflect.lisp,v 1.15 2007/09/08 14:48:59 youngde Exp $

(in-package "LISA.REFLECT")

;;; The code contained with the following MACROLET form courtesy of the PORT
;;; module, CLOCC project, http://clocc.sourceforge.net.

#+(or allegro clisp cmu cormanlisp lispworks lucid sbcl)
;; we use `macrolet' for speed - so please be careful about double evaluations
;; and mapping (you cannot map or funcall a macro, you know)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((class-slots* (class)
               #+allegro `(clos:class-slots ,class)
               #+clisp `(clos:class-slots ,class)
               #+cmu `(pcl::class-slots ,class)
               #+cormanlisp `(cl:class-slots ,class)
               #+lispworks `(hcl::class-slots ,class)
               #+lucid `(clos:class-slots ,class)
               #+sbcl `(sb-pcl::class-slots ,class))
             (class-slots1 (obj)
               `(class-slots*
                 (typecase ,obj
                   (class ,obj)
                   (symbol (find-class ,obj))
                   (t (class-of ,obj)))))
             (slot-name (slot)
               #+(and allegro (not (version>= 6))) `(clos::slotd-name ,slot)
               #+(and allegro (version>= 6)) `(clos:slot-definition-name ,slot)
               #+clisp `(clos:slot-definition-name ,slot)
               #+cmu `(slot-value ,slot 'pcl::name)
               #+cormanlisp `(getf ,slot :name)
               #+lispworks `(hcl::slot-definition-name ,slot)
               #+lucid `(clos:slot-definition-name ,slot)
               #+sbcl `(slot-value ,slot 'sb-pcl::name))
             (slot-initargs (slot)
               #+(and allegro (not (version>= 6))) `(clos::slotd-initargs ,slot)
               #+(and allegro (version>= 6))
               `(clos:slot-definition-initargs ,slot)
               #+clisp `(clos:slot-definition-initargs ,slot)
               #+cmu `(slot-value ,slot 'pcl::initargs)
               #+cormanlisp `(getf ,slot :initargs)
               #+lispworks `(hcl::slot-definition-initargs ,slot)
               #+lucid `(clos:slot-definition-initargs ,slot)
               #+sbcl `(slot-value ,slot 'sb-pcl::initargs))
             (slot-one-initarg (slot) `(car (slot-initargs ,slot)))
             (slot-alloc (slot)
               #+(and allegro (not (version>= 6)))
               `(clos::slotd-allocation ,slot)
               #+(and allegro (version>= 6))
               `(clos:slot-definition-allocation ,slot)
               #+clisp `(clos:slot-definition-allocation ,slot)
               #+cmu `(pcl::slot-definition-allocation ,slot)
               #+cormanlisp `(getf ,slot :allocation)
               #+lispworks `(hcl::slot-definition-allocation ,slot)
               #+lucid `(clos:slot-definition-allocation ,slot)
               #+sbcl `(sb-pcl::slot-definition-allocation ,slot)))

    (defun class-slot-list (class &optional (all t))
      "Return the list of slots of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
      (unless (class-finalized-p class)
        (finalize-inheritance class))
      (mapcan (if all (utils:compose list slot-name)
                (lambda (slot)
                  (when (eq (slot-alloc slot) :instance)
                    (list (slot-name slot)))))
              (class-slots1 class)))

    (defun class-slot-initargs (class &optional (all t))
      "Return the list of initargs of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
initargs for all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
      (mapcan (if all (utils:compose list slot-one-initarg)
                (lambda (slot)
                  (when (eq (slot-alloc slot) :instance)
                    (list (slot-one-initarg slot)))))
              (class-slots1 class)))))

#+(or clisp cmu)
(defun ensure-class (name &key (direct-superclasses '()))
  (eval `(defclass ,name ,direct-superclasses ())))

#+clisp
(defun class-finalized-p (class)
  (clos:class-finalized-p class))

#+clisp
(defun finalize-inheritance (class)
  (clos:finalize-inheritance class))

(defun is-standard-classp (class)
  (or (eq (class-name class) 'standard-object)
       (eq (class-name class) t)))

(defun find-direct-superclasses (class)
  #+:sbcl
  (remove-if #'is-standard-classp (sb-mop:class-direct-superclasses class))
  #-:sbcl
  (remove-if #'is-standard-classp (clos:class-direct-superclasses class)))
             
(defun class-all-superclasses (class-or-symbol)
  (labels ((find-superclasses (class-list superclass-list)
             (let ((class (first class-list)))
               (if (or (null class-list)
                       (is-standard-classp class))
                   superclass-list
                 (find-superclasses 
                  (find-direct-superclasses class)
                  (find-superclasses 
                   (rest class-list) (pushnew class superclass-list)))))))
    (let ((class
           (if (symbolp class-or-symbol)
               (find-class class-or-symbol)
             class-or-symbol)))
      (nreverse (find-superclasses (find-direct-superclasses class) nil)))))
