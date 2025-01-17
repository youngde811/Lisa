;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :lisa-system)
    (defpackage "LISA-SYSTEM"
      (:use "COMMON-LISP" "ASDF"))))

(in-package :lisa-system)

(defsystem lisa-logger
  :name "Lisa-Logger"
  :author "David E. Young"
  :maintainer "David E. Young"
  :licence "MIT"
  :description "The logging package for Lisa, an expert system shell written in Common Lisp"
  :depends-on ("log4cl" "lisa")
  :components
  ((:module src
    :components
    ((:module logger
      :components
      ((:file "logger")))))))

(pushnew :lisa-logger.asdf *features*)
(pushnew :log4cl *features*)
