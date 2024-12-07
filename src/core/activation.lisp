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

;; Description: This class represents an activation of a rule.

(in-package :lisa)

(defvar *activation-timestamp* 0)

(defclass activation ()
  ((rule :initarg :rule
         :initform nil
         :reader activation-rule)
   (tokens :initarg :tokens
           :initform nil
           :reader activation-tokens)
   (timestamp :initform (incf *activation-timestamp*)
              :reader activation-timestamp)
   (eligible :initform t
             :accessor activation-eligible))
  (:documentation
   "Represents a rule activation."))

(defmethod activation-priority ((self activation))
  (rule-salience (activation-rule self)))

(defmethod fire-activation ((self activation))
  (trace-firing self)
  (fire-rule (activation-rule self) (activation-tokens self)))

(defun eligible-p (activation)
  (activation-eligible activation))

(defun inactive-p (activation)
  (not (eligible-p activation)))

(defun activation-fact-list (activation &key (detailp nil))
  (token-make-fact-list (activation-tokens activation) :detailp detailp))

(defmethod print-object ((self activation) strm)
  (let ((tokens (activation-tokens self))
        (rule (activation-rule self)))
    (print-unreadable-object (self strm :identity t :type t)
      (format strm "(~A ~A ; salience = ~D)"
              (rule-name rule)
              (mapcar #'fact-symbolic-id 
                      (token-make-fact-list tokens))
              (rule-salience rule)))))

(defmethod hash-key ((self activation))
  (hash-key (activation-tokens self)))

(defun make-activation (rule tokens)
  (make-instance 'activation :rule rule :tokens tokens))

