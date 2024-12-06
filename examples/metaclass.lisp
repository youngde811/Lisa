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

(in-package "CL-USER")

(defclass standard-kb-class (standard-class) ())

(defmethod initialize-instance :after ((self standard-kb-class) &rest initargs)
  (dolist (slot (slot-value self 'clos::direct-slots))
    (dolist (writer (clos:slot-definition-writers slot))
      (let* ((gf (ensure-generic-function writer))
             (method-class
              (generic-function-method-class gf)))
        (multiple-value-bind (body initargs)
            (clos:make-method-lambda
             gf
             (class-prototype method-class)
             '(new-value object)
             nil
             `(format t "setting slot ~S to ~S~%" ',(clos:slot-definition-name slot) new-value))
          (clos:add-method
           gf
           (apply #'make-instance method-class
                  :function (compile nil body)
                  :specializers
                  `(,(find-class t) ,self)
                  :qualifiers '(:after)
                  :lambda-list '(value object)
                  initargs)))))))

(defmethod validate-superclass ((class standard-kb-class)
                                (superclass standard-class))
  t)

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name)
   (age :initarg :age
        :initform 100
        :accessor frodo-age))
  (:metaclass standard-kb-class))

(defparameter *frodo* (make-instance 'frodo :name 'frodo))
