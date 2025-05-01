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

(defclass token ()
  ((facts :initform (list)
          :accessor token-stack)
   (not-counter :initform 0
                :accessor token-not-counter)
   (exists-counter :initform 0
                   :accessor token-exists-counter)
   (hash-code :initform (list)
              :accessor token-hash-code)
   (fact-count :initform 0
               :type (unsigned-byte 64)
               :accessor token-fact-count))) ; big performance optimization

(defclass add-token (token) ())
(defclass remove-token (token) ())
(defclass reset-token (token) ())

(defun token-increment-exists-counter (token)
  (incf (token-exists-counter token)))

(defun token-decrement-exists-counter (token)
  (cl:assert (plusp (token-exists-counter token)) nil
    "The EXISTS join node logic is busted.")
  (decf (token-exists-counter token)))

(defun token-increment-not-counter (token)
  (values token (incf (token-not-counter token))))

(defun token-decrement-not-counter (token)
  (cl:assert (plusp (token-not-counter token)) nil
    "The negated join node logic is busted.")
  (values token (decf (token-not-counter token))))

(defun token-negated-p (token)
  (plusp (token-not-counter token)))

(defun token-make-fact-list (token &key (detailp t) (debugp nil))
  (let ((facts (list)))
    (dolist (item (token-stack token))
      (if debugp
          (push item facts)
        (when (typep item 'fact)
          (push (if detailp item (fact-symbolic-id item)) 
                facts))))
    (nreverse facts)))

(defun token-find-fact (token address)
  (nth address (token-stack token)))

(defun token-top-fact (token)
  (with-slots ((fact-stack facts)
               (fact-count fact-count)) token
    (declare (type fixnum fact-count))
    (first fact-stack)))

;;; Using WITH-SLOTS yields a 2x improvement in CPU usage during profiling.

(defun token-push-fact (token fact)
  (with-slots ((fact-stack facts)
               (fact-count fact-count)
               (hash-code hash-code)) token
    (push fact fact-stack)
    (push fact hash-code)
    (incf fact-count))
  token)

(defun token-pop-fact (token)
  (declare (type token token))
  (with-slots ((fact-stack facts)
               (hash-code hash-code)
               (fact-count fact-count)) token
    (declare (type fixnum fact-count))
    (unless (zerop fact-count)
      (pop hash-code)
      (decf fact-count)
      (pop fact-stack))))

(defun replicate-token (token &key (token-class nil))
  (let ((new-token
         (make-instance (if token-class
                            (find-class token-class)
                          (class-of token)))))
    (with-slots ((active-fact-stack facts)) token
      (dolist (item (reverse (token-stack token)))
        (token-push-fact new-token item)))
    new-token))

(defmethod hash-key ((self token))
  (token-hash-code self))

(defmethod make-add-token ((fact fact))
  (token-push-fact (make-instance 'add-token) fact))

(defmethod make-remove-token ((fact fact))
  (token-push-fact (make-instance 'remove-token) fact))

(defmethod make-remove-token ((token token))
  (replicate-token token :token-class 'remove-token))

(defmethod make-reset-token ((fact t))
  (token-push-fact (make-instance 'reset-token) t))
