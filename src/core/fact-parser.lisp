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
       
