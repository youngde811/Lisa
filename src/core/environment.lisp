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

;; Description: Defines the standard Lisa environment.

(in-package :lisa)

(defvar *default-engine* nil
  "The currently active inference engine.")

(defun use-default-engine ()
  "Create and make available a default instance of the inference engine. Use
    this function when you want a basic, single-threaded Lisa environment."
  (when (null *default-engine*)
    (setf *default-engine* (make-inference-engine)))
  (values *default-engine*))

(defun use-engine (engine)
  "Make ENGINE the default inference engine. Use this function with great care
  in an MP environment."
  (setf *default-engine* engine))

(defun current-engine (&optional (errorp t))
  "Returns the currently-active inference engine. Usually only invoked by code
  running within the context of WITH-INFERENCE-ENGINE."
  (when errorp
    (cl:assert (not (null *default-engine*)) (*default-engine*)
               "The current inference engine has not been established."))
  (values *default-engine*))

(defmacro with-inference-engine ((engine) &body body)
  "Evaluates BODY within the context of the inference engine ENGINE. This
    macro is MP-safe."
  `(let ((*default-engine* ,engine))
    (progn ,@body)))

(defun clear-environment (engine)
  "Completely resets the inference engine ENGINE."
  (clear-engine engine)
  (values))
