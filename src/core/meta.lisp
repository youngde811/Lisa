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

;; Description: Meta operations that Lisa uses to support the manipulation of
;; facts and instances.

;; NB: A note on terminology. We make the distinction here between symbolic
;; slot names and effective slot names. The former refers to an internal
;; symbol, created by Lisa, used to identify fact slots within rules; the
;; latter refers to the actual, package-qualified slot name.

(in-package :lisa)

(defun get-class-name (meta-object)
  (fact-meta-object-class-name meta-object))

(defun get-slot-list (meta-object)
  (fact-meta-object-slot-list meta-object))

(defun get-superclasses (meta-object)
  (fact-meta-object-superclasses meta-object))

(defun find-meta-fact (symbolic-name &optional (errorp t))
  "Locates the META-FACT instance associated with SYMBOLIC-NAME. If ERRORP is
  non-nil, signals an error if no binding is found."
  (let ((meta-fact (find-meta-object (inference-engine) symbolic-name)))
    (when (and errorp (null meta-fact))
      (log:fatal "This fact does not have a registered meta class: ~S" symbolic-name)
      (error "This fact does not have a registered meta class: ~S" symbolic-name))
    meta-fact))

;;; Corrected version courtesy of Aneil Mallavarapu...

(defun acquire-meta-data (actual-name)
  (labels ((build-meta-object (class all-superclasses) ;  NEW LINE (AM 9/19/03)
             (let* ((class-name (class-name class))
                    (meta-data
                     (make-fact-meta-object
                      :class-name class-name
                      :slot-list (reflect:class-slot-list class)
                      :superclasses all-superclasses))) ; new line (AM 9/19/03)
               (register-meta-object (inference-engine) class-name meta-data)
               meta-data))
           (examine-class (class-object)
             (let ((superclasses
                    (if *consider-taxonomy-when-reasoning*
                        (reflect:class-all-superclasses class-object) ; NEW LINE (AM 9/19/03)
                      nil)))
               (build-meta-object class-object superclasses)
               (dolist (super superclasses)
                 (examine-class super)))))
    (examine-class (find-class actual-name))))

;;; Corrected version courtesy of Aneil Mallavarapu...

(defun import-class-specification (class-name)
  (labels ((import-class-object (class-object) ; defined this internal function
             (let ((class-symbols (list class-name)))
               (dolist (slot-name (reflect:class-slot-list class-object))
                 (push slot-name class-symbols))
               (import class-symbols)
               (when *consider-taxonomy-when-reasoning*
                 (dolist (ancestor (reflect:find-direct-superclasses class-object))
                   (import-class-object ancestor))) ; changed to import-class-object
               class-object)))
    (import-class-object (find-class class-name))))

(defun ensure-meta-data-exists (class-name)
  (flet ((ensure-class-definition ()
           (loop
             (when (find-class class-name nil)
               (acquire-meta-data class-name)
               (return))
             (cerror "Enter a template definition now."
                     "Lisa doesn't know about the template named by (~S)." class-name)
             (format t "Enter a DEFTEMPLATE form: ")
             (eval (read))
             (fresh-line))))
    (let ((meta-data (find-meta-object (inference-engine) class-name)))
      (when (null meta-data)
        (ensure-class-definition)
        (setf meta-data 
          (find-meta-object (inference-engine) class-name)))
      meta-data)))
