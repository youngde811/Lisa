;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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

;;; File: lisa.asd

;;; Description: Lisa's ASDF system definition file. To use it, you must have asdf loaded; Lisa
;;; provides a copy in "lisa:misc;asdf.lisp".

;;; Assuming a loaded asdf, this is the easiest way to install Lisa:
;;;   (push <lisa root directory> asdf:*central-registry*)
;;;   (asdf:operate 'asdf:load-op :lisa)

;;; $Id: lisa.asd,v 1.7 2007/09/11 21:14:07 youngde Exp $

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :lisa-system)
    (defpackage "LISA-SYSTEM"
      (:use "COMMON-LISP" "ASDF"))))

(in-package :lisa-system)

(defsystem lisa
    :name "Lisa"
    :author "David E. Young"
    :maintainer "David E. Young"
    :licence "LGPL"
    :description "The Lisa expert system shell"
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
                         (:file "rule")
                         (:file "pattern")
                         (:file "rule-parser")
                         (:file "fact-parser")
                         (:file "language")
                         (:file "tms-support")
                         (:file "rete")
                         (:file "belief-interface")
                         (:file "meta")
                         (:file "binding")
                         (:file "token")
                         (:file "retrieve"))
                        :serial t)
               (:module implementations
                        :components
                        ((:file "workarounds")
                         #+:lispworks
                         (:file "lispworks-auto-notify")
                         #+:cmucl
                         (:file "cmucl-auto-notify")
                         #+:allegro
                         (:file "allegro-auto-notify"))
                        :serial t)
               (:module rete
                        :pathname "rete/reference/"
                        :components
                        ((:file "node-tests")
                         (:file "shared-node")
                         (:file "successor")
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
               (:module config
                        :components
                        ((:file "config")
                         (:file "epilogue"))
                        :serial t))
              :serial t)))

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
