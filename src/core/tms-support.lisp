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

;; Description: Support functions for Lisa's Truth Maintenance System (TMS).

(in-package :lisa)

(defvar *scheduled-dependencies*)

(define-symbol-macro scheduled-dependencies *scheduled-dependencies*)

(defun add-logical-dependency (rete fact dependency-set)
  (setf (gethash dependency-set (rete-dependency-table rete))
    (push fact (gethash dependency-set (rete-dependency-table rete)))))

(defun find-logical-dependencies (rete dependency-set)
  (gethash dependency-set (rete-dependency-table rete)))

(defun make-dependency-set (tokens marker)
  (let ((dependencies (list)))
    (loop for i from 1 to marker
        do (push (token-find-fact tokens i) dependencies))
    (nreverse dependencies)))

(defun schedule-dependency-removal (dependency-set)
  (push dependency-set scheduled-dependencies))

(defmacro with-truth-maintenance ((rete) &body body)
  (let ((rval (gensym)))
    `(let* ((*scheduled-dependencies* (list))
            (,rval
             (progn ,@body)))
       (dolist (dependency scheduled-dependencies)
         (with-accessors ((table rete-dependency-table)) ,rete
           (dolist (dependent-fact
                       (gethash dependency table)
                     (remhash dependency table))
             (retract-fact ,rete dependent-fact))))
       ,rval)))
