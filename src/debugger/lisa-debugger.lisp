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

;;; File: debugger.lisp
;;; Description: The LISA debugger.

;;; $Id: lisa-debugger.lisp,v 1.11 2007/09/07 21:34:37 youngde Exp $

(in-package "LISA")

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
