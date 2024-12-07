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

;; Description: Allegro-specific implementation of Lisa's auto-notification
;; mechanism, whereby changes to the slot values of CLOS instances, outside
;; of Lisa's control, are picked up via the MOP protocol and synchronized
;; with KB facts.

(in-package :lisa)

(defclass standard-kb-class (standard-class) ())

(defmethod make-instance :around ((self standard-kb-class) &rest initargs)
  (declare (ignore initargs))
  (let ((*ignore-this-instance* self))
    (call-next-method)))

(defmethod (setf mop:slot-value-using-class) :after (new-value (class standard-kb-class) instance slot)
  (declare (ignore new-value))
  (flet ((ignore-instance (object)
           (and (boundp '*ignore-this-instance*)
                (eq object *ignore-this-instance*))))
    (unless (ignore-instance class)
      (mark-instance-as-changed 
       instance :slot-id (clos:slot-definition-name slot)))))

(defmethod validate-superclass ((class standard-kb-class) (superclass standard-class))
  t)

(eval-when (:load-toplevel)
  (pushnew :lisa-autonotify *features*))
