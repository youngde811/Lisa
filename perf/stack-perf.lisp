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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "STACK-PERF")
    (defpackage "STACK-PERF"
      (:use "COMMON-LISP")
      (:export "TEST-DEFAULT-IMPL"
               "TEST-GP-IMPL"))))

(in-package #:stack-perf)

(defclass token ()
  ((facts :initform nil
          :initarg :stack
          :accessor token-facts)
   (hash-code :initform nil
              :accessor token-hash-code)
   (fact-count :initform 0
               :type (unsigned-byte 64)
               :accessor token-fact-count)))

(defclass fact ()
  ((name :initform nil
         :initarg :name
         :reader fact-name)))

(defun token-top-fact-default (token)
  (with-slots ((fact-vector facts)
               (fact-count fact-count)) token
    (declare (type fixnum fact-count))
    (aref fact-vector (1- fact-count))))

(defun token-push-fact-default (token fact)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (with-slots ((fact-vector facts)
               (fact-count fact-count)
               (hash-code hash-code)) token
    (declare (type fixnum fact-count))
    (vector-push-extend fact fact-vector)
    (push fact hash-code)
    (incf fact-count))
  token)

(defun token-pop-fact-default (token)
  (declare (type token token))
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (with-slots ((fact-vector facts)
               (fact-count fact-count)
               (hash-code hash-code)) token
    (declare (type fixnum fact-count))
    (unless (zerop (fill-pointer fact-vector))
      (pop hash-code)
      (aref fact-vector (decf (fill-pointer fact-vector))))))

(defun token-push-fact-gs (token fact)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (with-slots ((fact-stack facts)
               (hash-code hash-code)) token
    (grouping-stack:stack-push fact fact-stack)
    (push fact hash-code))
  token)

(defun token-pop-fact-gs (token)
  (declare (type token token))
  (with-slots ((fact-stack facts)
               (hash-code hash-code)) token
    (unless (zerop (grouping-stack:stack-size fact-stack))
      (pop hash-code)
      (grouping-stack:stack-pop fact-stack))))

(defun test-default-impl (&key (ntimes 100))
  (let ((token (make-instance 'token :stack (make-array 64 :initial-element nil :adjustable t :fill-pointer 0))))
    (dotimes (i ntimes)
      (token-push-fact-default token (make-instance 'fact :name (format nil "frodo-~D" i))))
    (dotimes (i ntimes)
      (token-pop-fact-default token))))

(defun test-gs-impl (&key (ntimes 100))
  (let ((token (make-instance 'token
                              :stack (grouping-stack:make-grouping-stack
                                      (make-instance 'grouping-stack:inactive-balancer)))))
    (dotimes (i ntimes)
      (token-push-fact-gs token (make-instance 'fact :name (format nil "frodo-~D" i))))
    (dotimes (i ntimes)
      (token-pop-fact-gs token))))
