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

;; Description: The LISA debugger.

(in-package :lisa)

(defvar *breakpoints* (make-hash-table))
(defvar *stepping* nil)
(defvar *read-eval-print*)
(defvar *suspended-rule*)
(defvar *tokens*)

(defmacro in-debugger-p ()
  `(cl:assert (boundp '*suspended-rule*) nil
     "The debugger must be running to use this function."))

#+LispWorks
(defmacro with-debugger-streams (&body body)
  `(let ((*standard-input* *standard-input*)
         (*standard-output* *standard-output*)
         (*terminal-io* *terminal-io*))
     (progn ,@body)))

#-LispWorks
(defmacro with-debugger-streams (&body body)
  `(let ((*terminal-io* *terminal-io*)
         (*standard-input* *terminal-io*)
         (*standard-output* *terminal-io*))
     (progn ,@body)))

(defun leave-debugger ()
  (setf *stepping* nil))

(defun has-breakpoint-p (rule)
  (gethash (rule-name rule) *breakpoints*))

(defun breakpoints ()
  (format t "Current breakpoints:~%")
  (loop for rule-name being the hash-value of *breakpoints*
      do (format t "  ~A~%" rule-name))
  (values))

(defun breakpoint-operation (rule-name op)
  (let ((rule (find-rule (inference-engine) rule-name)))
    (cond ((null rule)
           (format t "There's no rule by this name (~A)~%" rule-name))
          (t
           (funcall op (rule-name rule))))
    rule-name))

(defun set-break (rule-name)
  (breakpoint-operation 
   rule-name #'(lambda (rule-name)
                 (setf (gethash rule-name *breakpoints*)
                   rule-name)))
  rule-name)
                        
(defun clear-break (rule-name)
  (breakpoint-operation
   rule-name #'(lambda (rule-name)
                 (remhash rule-name *breakpoints*)))
  rule-name)

(defun clear-breaks ()
  (clrhash *breakpoints*)
  nil)

(defun next ()
  (in-debugger-p)
  (setf *stepping* t)
  (setf *read-eval-print* nil)
  (values))

(defun resume ()
  (in-debugger-p)
  (setf *read-eval-print* nil)
  (setf *stepping* nil)
  (values))

(defun instance (fact)
  (find-instance-of-fact fact))

(defun token (index)
  (in-debugger-p)
  (cl:assert (and (not (minusp index))
                  (< index (token-fact-count *tokens*)))
      nil "The token index isn't valid.")
  (let ((fact (token-find-fact *tokens* index)))
    (cond ((typep fact 'fact)
           fact)
          (t
           (format t "The index ~D references a non-fact object." index)
           nil))))

(defun tokens (&key (verbose nil))
  (in-debugger-p)
  (format t "Token stack for ~A:~%" (rule-name (rule)))
  (do* ((facts (token-make-fact-list *tokens* :debugp t) (rest facts))
        (fact (first facts) (first facts))
        (index 0 (incf index)))
      ((endp facts))
    (when (typep fact 'fact)
      (if verbose
          (format t "  [~D] ~S~%" index fact)
        (format t "  [~D] ~A, ~A~%"
                index
                (fact-symbolic-id fact)
                (fact-name fact)))))
  (values))

(defun bindings ()
  (in-debugger-p)
  (format t "Effective bindings for ~A:~%" (rule-name (rule)))
  (dolist (binding (rule-binding-set (rule)))
    (format t "  ~A: ~S~%"
            (binding-variable binding)
            (if (pattern-binding-p binding)
                (token-find-fact *tokens* (binding-address binding))
              (get-slot-value
               (token-find-fact *tokens* (binding-address binding))
               (binding-slot-name binding)))))
  (values))

(defun debugger-repl ()
  (with-debugger-streams
   (do ((*read-eval-print* t)
        (count 0 (incf count)))
       ((not *read-eval-print*) count)
     (handler-case
         (progn
           (format t "LISA-DEBUG[~D]: " count)
           (force-output)
           (print (eval (read-from-string (read-line))))
           (terpri))
       (error (e)
              (cerror "Remain in the LISA debugger." e)
              (unless (yes-or-no-p "Remain in the debugger? ")
                (leave-debugger)
                (setf *read-eval-print* nil)))))))

(defmethod fire-rule :around ((self rule) tokens)
  (when (or *stepping*
            (has-breakpoint-p self))
    (let ((*active-rule* self)
          (*suspended-rule* self)
          (*tokens* tokens))
      (format t "Stopping in rule ~S~%" (rule-name self))
      (debugger-repl)))
  (call-next-method))

(defmethod run-engine :after ((self rete) &optional step)
  (leave-debugger))

(defmethod forget-rule :before ((self rete) (rule-name symbol))
  (clear-break rule-name))

(provide 'lisa-debugger)
