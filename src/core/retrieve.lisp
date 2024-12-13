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

(defvar *query-result* nil
  "Holds the results of query firings.")

(defun run-query (query-rule)
  "Runs a query (RULE instance), and returns both the value of *QUERY-RESULT*
  and the query name itself."
  (declare (ignorable query-rule))
  (let ((*query-result* (list)))
    (assert (query-fact))
    (run)
    *query-result*))

(defmacro defquery (name &body body)
  "Defines a new query identified by the symbol NAME."
  `(define-rule ,name ',body))

;;; Queries fired by RETRIEVE collect their results in the special variable
;;; *QUERY-RESULT*. As an example, one firing of this query, 
;;;
;;;   (retrieve (?x ?y) 
;;;     (?x (rocky (name ?name)))
;;;     (?y (hobbit (name ?name))))
;;;
;;; will produce a result similar to,
;;;
;;; (((?X . #<ROCKY @ #x7147b70a>) (?Y . #<HOBBIT @ #x7147b722>)))

#+nil
(defmacro retrieve ((&rest varlist) &body body)
  (flet ((make-query-binding (var)
           `(cons ',var ,var)))
    (let ((query-name (gensym))
          (query (gensym)))
      `(with-inference-engine
          ((make-query-engine (inference-engine)))
         (let* ((,query-name (gensym))
                (,query
                 (defquery ',query-name
                           (query-fact)
                           ,@body
                           =>
                           (push (list ,@(mapcar #'make-query-binding varlist))
                                 *query-result*))))
           (run-query ,query))))))

(defmacro retrieve ((&rest varlist) &body body)
  (let ((query-name (gensym))
        (query (gensym)))
    `(with-inference-engine
         ((make-query-engine (inference-engine)))
       (let* ((,query-name (gensym))
              (,query
                (defquery ',query-name
                  (query-fact)
                  ,@body
                  =>
                  (push (list ,@(mapcar #'(lambda (var)
                                            var)
                                        varlist))
                        *query-result*))))
         (run-query ,query)))))

(defmacro with-simple-query ((var value) query &body body)
  "For each variable/instance pair in a query result, invoke BODY with VAR
  bound to the query variable and VALUE bound to the instance."
  (let ((result (gensym)))
    `(let ((,result ,query))
       (dolist (match ,result)
         (dolist (binding match)
           (let ((,var (car binding))
                 (,value (cdr binding)))
             ,@body))))))
