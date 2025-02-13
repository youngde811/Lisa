;;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.

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

(deftemplate initial-fact ())

(deftemplate query-fact ())

;;; This macro is courtesy of Paul Werkowski. A very nice idea.

(defmacro define-lisa-lisp ()
  (flet ((externals-of (pkg)
           (loop for s being each external-symbol in pkg collect s)))
    (let* ((lisa-externs (externals-of "LISA"))
           (lisa-shadows (intersection (package-shadowing-symbols "LISA")
                                       lisa-externs))
           (cl-externs (externals-of "COMMON-LISP")))
      `(defpackage "LISA-LISP"
         (:use "COMMON-LISP")
         (:shadowing-import-from "LISA" ,@lisa-shadows)
         (:import-from "LISA" ,@(set-difference lisa-externs lisa-shadows))
         (:export ,@cl-externs)
         (:export ,@lisa-externs)))))

(eval-when (:load-toplevel :execute)
  (make-default-inference-engine)
  (setf *active-context* (initial-context (inference-engine)))
  (define-lisa-lisp)
  (when (use-fancy-assert)
    (set-dispatch-macro-character
     #\# #\? #'(lambda (strm subchar arg)
                 (declare (ignore subchar arg))
                 (list 'identity (read strm t nil t)))))
  (pushnew :lisa *features*)
  (pushnew :lisa3.9.1 *features*))


