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

(defvar *active-rule* nil)
(defvar *active-engine* nil)
(defvar *active-tokens* nil)
(defvar *active-context* nil)
(defvar *ignore-this-instance*)

(defmacro with-auto-notify ((var instance) &body body)
  `(let* ((,var ,instance)
          (*ignore-this-instance* ,var))
     ,@body))

(defgeneric make-rete-network (&rest args &key &allow-other-keys))

(defun active-context ()
  *active-context*)

(defun active-tokens ()
  *active-tokens*)

(defun active-rule ()
  *active-rule*)

(defun active-engine ()
  *active-engine*)

(defun in-rule-firing-p ()
  (not (null (active-rule))))

(defgeneric equals (a b))
(defgeneric slot-value-of-instance (object slot-name))
(defgeneric (setf slot-value-of-instance) (new-value object slot-name))

(defvar *consider-taxonomy-when-reasoning* nil)
(defvar *allow-duplicate-facts* t)
(defvar *use-fancy-assert* t)
(defvar *clear-handlers* nil)

(defun consider-taxonomy ()
  *consider-taxonomy-when-reasoning*)

(defsetf consider-taxonomy () (new-value)
  `(setf *consider-taxonomy-when-reasoning* ,new-value))

(defun allow-duplicate-facts ()
  *allow-duplicate-facts*)

(defsetf allow-duplicate-facts () (new-value)
  `(setf *allow-duplicate-facts* ,new-value))

(defun use-fancy-assert ()
  *use-fancy-assert*)

(defsetf use-fancy-assert () (new-value)
  `(setf *use-fancy-assert* ,new-value))

(defclass inference-engine-object () ())

(defmacro register-clear-handler (tag func)
  `(eval-when (:load-toplevel)
     (unless (assoc ,tag *clear-handlers* :test #'string=)
       (setf *clear-handlers*
         (acons ,tag ,func *clear-handlers*)))))

(defun clear-system-environment ()
  (mapc #'(lambda (assoc)
            (funcall (cdr assoc)))
        *clear-handlers*)
  t)

(defun clear-environment-handlers ()
  (setf *clear-handlers* nil))

(defun variable-p (obj)
  (and (symbolp obj)
       (char= (schar (symbol-name obj) 0) #\?)))

(defmacro starts-with-? (sym)
  `(eq (aref (symbol-name ,sym) 0) #\?))

(defmacro variablep (sym)
  `(variable-p ,sym))

(defmacro quotablep (obj)
  `(and (symbolp ,obj)
        (not (starts-with-? ,obj))))

(defmacro literalp (sym)
  `(or (and (symbolp ,sym)
            (not (variablep ,sym))
            (not (null ,sym)))
       (numberp ,sym) (stringp ,sym)))

(defmacro multifieldp (val)
  `(and (listp ,val)
    (eq (first ,val) 'quote)))

(defmacro slot-valuep (val)
  `(or (literalp ,val)
       (consp ,val)
       (variablep ,val)))

(defmacro constraintp (constraint)
  `(or (null ,constraint)
       (literalp ,constraint)
       (consp ,constraint)))

(defun make-default-inference-engine ()
  (when (null *active-engine*)
    (setf *active-engine* (make-inference-engine)))
  *active-engine*)

(defun use-default-engine ()
  (warn "USE-DEFAULT-ENGINE is deprecated. Lisa now automatically creates a
  default instance of the inference engine at load time.")
  (when (null *active-engine*)
    (setf *active-engine* (make-inference-engine)))
  *active-engine*)

(defun current-engine (&optional (errorp t))
  "Returns the currently-active inference engine. Usually only invoked by code
  running within the context of WITH-INFERENCE-ENGINE."
  (when errorp
    (cl:assert (not (null *active-engine*)) (*active-engine*)
      "The current inference engine has not been established."))
  *active-engine*)

(defun inference-engine (&rest args)
  (apply #'current-engine args))

(defmacro with-inference-engine ((engine) &body body)
  "Evaluates BODY within the context of the inference engine ENGINE. This
    macro is MP-safe."
  `(let ((*active-engine* ,engine))
    (progn ,@body)))

(register-clear-handler
 "environment" 
 #'(lambda ()
     (setf *active-engine* (make-inference-engine))
     (setf *active-context* (find-context (inference-engine) :initial-context))))
