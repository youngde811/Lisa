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

(define-condition duplicate-fact (error)
  ((existing-fact :reader duplicate-fact-existing-fact
                  :initarg :existing-fact))
  (:report (lambda (condition strm)
             (declare (ignore strm))
             (log:warn "Lisa detected an attempt to assert a duplicate for: ~S"
                       (duplicate-fact-existing-fact condition)))))
                  
(define-condition parsing-error (error)
  ((text :initarg :text
         :initform nil
         :reader text)
   (location :initarg :location
             :initform nil
             :reader location))
  (:report (lambda (condition strm)
             (declare (ignore strm))
             (log:error "Parsing error: ~A" (text condition))
             (error t))))

(define-condition slot-parsing-error (parsing-error)
  ((slot-name :initarg :slot-name
              :initform nil
              :reader slot-name))
  (:report (lambda (condition strm)
             (declare (ignore strm))
             (log:error "Slot parsing error: slot ~A, pattern location ~A, text ~A"
                        (slot-name condition) (location condition) (text condition))
             (error t))))

(define-condition class-parsing-error (parsing-error)
  ((class-name :initarg :class-name
               :initform nil
               :reader class-name))
  (:report (lambda (condition strm)
             (declare (ignore strm))
             (log:error "Class parsing error: ~A, ~A" (class-name condition) (text condition)))))

(define-condition rule-parsing-error (parsing-error)
  ((rule-name :initarg :rule-name
              :initform nil
              :reader rule-name))
  (:report (lambda (condition strm)
             (declare (ignore strm))
             (log:error "Rule parsing error: rule name ~A, pattern location ~A, text ~A"
                        (rule-name condition) (location condition) (text condition)))))
