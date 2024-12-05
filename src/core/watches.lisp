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

(in-package :lisa)

(defvar *assert-fact* nil)
(defvar *retract-fact* nil)
(defvar *enable-activation* nil)
(defvar *disable-activation* nil)
(defvar *fire-rule* nil)
(defvar *watches* nil)

(defun watch-activation-detail (activation direction)
  (format *trace-output* "~A Activation: ~A : ~A~%"
          direction
          (rule-default-name (activation-rule activation))
          (activation-fact-list activation))
  (values))

(defun watch-enable-activation (activation)
  (watch-activation-detail activation "==>"))

(defun watch-disable-activation (activation)
  (watch-activation-detail activation "<=="))

(defun watch-rule-firing (activation)
  (let ((rule (activation-rule activation)))
    (format *trace-output* "FIRE ~D: ~A ~A~%"
            (rete-firing-count (rule-engine rule))
            (rule-default-name rule)
            (activation-fact-list activation))
    (values)))

(defun watch-fact-detail (fact direction)
  (format *trace-output* "~A ~A ~S~%"
          direction (fact-symbolic-id fact)
          (reconstruct-fact fact))
  (values))

(defun watch-assert (fact)
  (watch-fact-detail fact "==>"))

(defun watch-retract (fact)
  (watch-fact-detail fact "<=="))

(defun watch-event (event)
  (ecase event
    (:facts (setf *assert-fact* #'watch-assert)
            (setf *retract-fact* #'watch-retract))
    (:activations (setf *enable-activation* #'watch-enable-activation)
                  (setf *disable-activation* #'watch-disable-activation))
    (:rules (setf *fire-rule* #'watch-rule-firing))
    (:all (watch-event :facts)
          (watch-event :activations)
          (watch-event :rules)))
  (unless (eq event :all)
    (pushnew event *watches*))
  event)

(defun unwatch-event (event)
  (ecase event
    (:facts (setf *assert-fact* nil)
            (setf *retract-fact* nil))
    (:activations (setf *enable-activation* nil)
                  (setf *disable-activation* nil))
    (:rules (setf *fire-rule* nil))
    (:all (unwatch-event :facts)
          (unwatch-event :activations)
          (unwatch-event :rules)))
  (unless (eq event :all)
    (setf *watches*
      (delete event *watches*)))
  event)

(defun watches ()
  *watches*)

(defmacro trace-assert (fact)
  `(unless (null *assert-fact*)
     (funcall *assert-fact* ,fact)))

(defmacro trace-retract (fact)
  `(unless (null *retract-fact*)
     (funcall *retract-fact* ,fact)))

(defmacro trace-enable-activation (activation)
  `(unless (null *enable-activation*)
     (funcall *enable-activation* ,activation)))

(defmacro trace-disable-activation (activation)
  `(unless (null *disable-activation*)
     (funcall *disable-activation* ,activation)))

(defmacro trace-firing (activation)
  `(unless (null *fire-rule*)
     (funcall *fire-rule* ,activation)))
