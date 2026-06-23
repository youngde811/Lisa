(in-package :cl-user)

(defpackage :lisa-bridge
  (:use :common-lisp)
  (:export
   #:start
   #:stop
   #:*bridge-port*))