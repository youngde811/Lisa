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

(defclass context ()
  ((name :initarg :name
         :reader context-name)
   (rules :initform (make-hash-table :test #'equal)
          :reader context-rules)
   (strategy :initarg :strategy
             :reader context-strategy)))

(defmethod print-object ((self context) strm)
  (print-unreadable-object (self strm :type t)
    (if (initial-context-p self)
        (format strm "~S" "The Initial Context")
      (format strm "~A" (context-name self)))))

(defmethod find-rule-in-context ((self context) (rule-name string))
  (values (gethash rule-name (context-rules self))))

(defmethod find-rule-in-context ((self context) (rule-name symbol))
  (values (gethash (symbol-name rule-name) (context-rules self))))

(defun add-rule-to-context (context rule)
  (setf (gethash (symbol-name (rule-name rule)) (context-rules context))
    rule))

(defmethod conflict-set ((self context))
  (context-strategy self))

(defmethod remove-rule-from-context ((self context) (rule-name symbol))
  (remhash (symbol-name rule-name) (context-rules self)))

(defmethod remove-rule-from-context ((self context) (rule t))
  (remove-rule-from-context self (rule-name rule)))

(defun clear-activations (context)
  (remove-activations (context-strategy context)))

(defun context-activation-list (context)
  (list-activations (context-strategy context)))

(defun context-rule-list (context)
  (loop for rule being the hash-values of (context-rules context)
      collect rule))

(defun clear-context (context)
  (clear-activations context)
  (clrhash (context-rules context)))

(defun initial-context-p (context)
  (string= (context-name context) "INITIAL-CONTEXT"))

(defun make-context-name (defined-name)
  (typecase defined-name
    (symbol (symbol-name defined-name))
    (string defined-name)
    (otherwise
     (error "The context name must be a string designator."))))

(defmacro with-context (context &body body)
  `(let ((*active-context* ,context))
     ,@body))

(defmacro with-rule-name-parts ((context short-name long-name) 
                                symbolic-name &body body)
  (let ((qualifier (gensym))
        (rule-name (gensym)))
    `(let* ((,rule-name (symbol-name ,symbolic-name))
            (,qualifier (position #\. ,rule-name))
            (,context (if ,qualifier
                          (subseq ,rule-name 0 ,qualifier)
                        (symbol-name :initial-context)))
            (,short-name (if ,qualifier
                             (subseq ,rule-name (1+ ,qualifier))
                           ,rule-name))
            (,long-name (if ,qualifier
                            ,rule-name
                          (concatenate 'string ,context "." ,short-name))))
       ,@body)))

(defun make-context (name &key (strategy nil))
  (make-instance 'context
    :name (make-context-name name)
    :strategy (if (null strategy)
                  (make-breadth-first-strategy)
                strategy)))
