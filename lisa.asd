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

;; Description: Lisa's ASDF system definition file. To use it, you must have asdf loaded; Lisa
;; provides a copy in "lisa:misc;asdf.lisp".

;; Assuming a loaded asdf, this is the easiest way to install Lisa:
;;   (push <lisa root directory> asdf:*central-registry*)
;;   (asdf:operate 'asdf:load-op :lisa)

(in-package :cl-user)

#-asdf
(error "The ASDF package is required. Please load it first")

(defvar *install-root* (make-pathname :directory (pathname-directory *load-truename*)))

(push *install-root* asdf:*central-registry*)

;;; There's a bug in Lisa that is creating a symbol in the COMMON-LISP package. I need
;;; to track that down. Until then, we unlock that package in SBCL.

#+sbcl
(progn
  (sb-ext:unlock-package :common-lisp)
  (sb-ext:unlock-package :cl-user))

(asdf:defsystem lisa
  :name "Lisa"
  :author "David E. Young"
  :maintainer "David E. Young"
  :licence "MIT"
  :description "The Lisa Expert System Shell"
  :depends-on ("log4cl")
  :components
  ((:module src
    :components
    ((:module packages
      :components
      ((:file "pkgdecl")))
     (:module utils
      :components
      ((:file "compose")
       (:file "utils"))
      :serial t)
     (:module belief-systems
      :components
      ((:file "belief")
       (:file "certainty-factors"))
      :serial t)
     (:module reflect
      :components
      ((:file "reflect")))
     (:module logger
      :components
      ((:file "logger")))
     (:module grouping-stack
      :components
      ((:file "package")
       (:file "item")
       (:file "stack")
       (:file "balancer"))
      :serial t)
     (:module core
      :components
      ((:file "preamble")
       (:file "conditions")
       (:file "deffacts")
       (:file "fact")
       (:file "watches")
       (:file "activation")
       (:file "heap")
       (:file "conflict-resolution-strategies")
       (:file "context")
       (:file "pattern")
       (:file "rule")
       (:file "binding")
       (:file "rule-parser")
       (:file "fact-parser")
       (:file "language")
       (:file "tms-support")
       (:file "rete")
       (:file "belief-interface")
       (:file "meta")
       (:file "token")
       (:file "retrieve"))
      :serial t)
     (:module rete
      :pathname "rete/reference/"
      :components
      ((:file "node-tests")
       (:file "successor")
       (:file "shared-node")
       (:file "node-pair")
       (:file "terminal-node")
       (:file "node1")
       (:file "join-node")
       (:file "node2")
       (:file "node2-not")
       (:file "node2-test")
       (:file "node2-exists")
       (:file "rete-compiler")
       (:file "tms")
       (:file "network-ops")
       (:file "network-crawler"))
      :serial t)
     #+:use-auto-notify
     (:module auto-notify
      :pathname "implementations"
      :components
      (
       #+:sbcl
       (:file "sbcl-auto-notify")
       #+:lispworks
       (:file "lispworks-auto-notify")
       #+:cmucl
       (:file "cmucl-auto-notify")
       #+:allegro
       (:file "allegro-auto-notify"))
      :serial t)
     (:module config
      :components
      ((:file "config")
       (:file "epilogue"))
      :serial t))
    :serial t)))

(pushnew :lisa.asdf *features*)
(pushnew :log4cl *features*)

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
        ("contrib;**;" ,(make-lisa-path "contrib/**/"))))

(defun lisa-debugger ()
  (translate-logical-pathname "lisa:debugger;lisa-debugger.lisp"))

;;; Sets up the environment so folks can use the non-portable form of REQUIRE
;;; with some implementations...

#+:allegro
(setf system:*require-search-list*
      (append system:*require-search-list*
              `(:newest ,(lisa-debugger))))

#+:clisp
(setf custom:*load-paths*
      (append custom:*load-paths* `(,(lisa-debugger))))

#+:openmcl
(pushnew (pathname-directory (lisa-debugger)) ccl:*module-search-path* :test #'equal)

#+:lispworks
(let ((loadable-modules `(("lisa-debugger" . ,(lisa-debugger)))))
  (lw:defadvice (require lisa-require :around)
      (module-name &optional pathname)
    (let ((lisa-module
            (find module-name loadable-modules
                  :test #'string=
                  :key #'car)))
      (if (null lisa-module)
          (lw:call-next-advice module-name pathname)
        (lw:call-next-advice module-name (cdr lisa-module))))))
