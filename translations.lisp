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

;; Description: Logical-pathname translations.

(in-package :cl-user)

(defvar *lisa-root-pathname*
  (make-pathname :directory
                 (pathname-directory *load-truename*)
                 :host (pathname-host *load-truename*)
                 :device (pathname-device *load-truename*)))

(defun make-lisa-path (relative-path)
  (concatenate 'string (namestring *lisa-root-pathname*)
               relative-path))

(setf (logical-pathname-translations "lisa")
      `(("src;**;" ,(make-lisa-path "src/**/"))
        ("lib;**;*.*" ,(make-lisa-path "lib/**/"))
        ("config;*.*" ,(make-lisa-path "config/"))
        ("debugger;*.*" ,(make-lisa-path "src/debugger/"))
        ("examples;*.*", (make-lisa-path "examples/**"))
        ("logging;**;*", (make-lisa-path "logging/cl-grip/**"))
        ("contrib;**;" ,(make-lisa-path "contrib/**/"))))
