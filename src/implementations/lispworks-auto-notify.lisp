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

;; Description: Lispworks-specific code for Lisa's auto notification
;; mechanism, whereby changes to the slot values of CLOS instances, outside
;; of Lisa's control, are picked up via the MOP protocol and synchronized
;; with KB facts.

(in-package :lisa)

(defclass standard-kb-class (standard-class) ())

(defun lispworks-respond-to-slot-change (instance slot-name)
  (flet ((ignore-instance (object)
           (and (boundp '*ignore-this-instance*)
                (eq object *ignore-this-instance*))))
    (unless (ignore-instance instance)
      (mark-instance-as-changed instance :slot-id slot-name))))
  
(defmethod initialize-instance :after ((self standard-kb-class) &rest initargs) 
  (dolist (slot (clos:class-direct-slots self))
    (dolist (writer (clos:slot-definition-writers slot))
      (let* ((gf (ensure-generic-function writer))
             (method-class (clos:generic-function-method-class gf)))
        (multiple-value-bind (body initargs)
            (clos:make-method-lambda gf (clos:class-prototype method-class) '(new-value object)
                                     nil
                                     `(lispworks-respond-to-slot-change
                                       object ',(clos:slot-definition-name slot)))
          (clos:add-method gf
                           (apply #'make-instance method-class
                                  :function (compile nil body)
                                  :specializers `(,(find-class t) ,self)
                                  :qualifiers '(:after)
                                  :lambda-list '(value object)
                                  initargs)))))))

(defmethod validate-superclass ((class standard-kb-class) (superclass standard-class))
  t)

(eval-when (:load-toplevel)
  (pushnew :lisa-autonotify *features*))
