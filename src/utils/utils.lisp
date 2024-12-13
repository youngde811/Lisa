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

;; Description: Miscellaneous utility functions.

(in-package "LISA.UTILS")

;;; This version of FIND-BEFORE courtesy of Bob Bane, Global Science and
;;; Technology...

(defun find-before (item sequence &key (test #'eql))
  "Returns both that portion of SEQUENCE that occurs before ITEM and
  the rest of SEQUENCE anchored at ITEM, or NIL otherwise."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (labels ((find-item (obj seq test val valend)
	     (let ((item (first seq)))
               (cond ((null seq)
                      (values nil nil))
                     ((funcall test obj item)
                      (values val seq))
                     (t
                      (let ((newend `(,item)))
                        (nconc valend newend)
                        (find-item obj (rest seq) test val newend)))))))
    (if (funcall test item (car sequence))
        (values nil sequence)
      (let ((head (list (car sequence))))
        (find-item item (cdr sequence) test head head)))))

(defun find-after (item sequence &key (test #'eql))
  "Returns that portion of SEQUENCE that occurs after ITEM, or NIL
  otherwise."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cond ((null sequence)
         (values nil))
        ((funcall test item (first sequence))
         (rest sequence))
        (t (find-after item (rest sequence) :test test))))

(defun find-if-after (predicate sequence)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cond ((null sequence)
         (values nil))
        ((funcall predicate (first sequence))
         (rest sequence))
        (t
         (find-if-after predicate (rest sequence)))))

(defun lsthash (func ht)
  "Applies FUNC to each entry in hashtable HT and, if FUNC so
  indicates, appends the object to a LIST. If NIL is an acceptable
  object, then FUNC should return two values; NIL and T."
  (let ((seq (list)))
    (maphash #'(lambda (key val)
                 (multiple-value-bind (obj use-p)
                     (funcall func key val)
                   (unless (and (null obj)
                                (not use-p))
                     (push obj seq)))) ht)
    (values seq)))

(defun collect (predicate list)
  (let ((collection (list)))
    (dolist (obj list)
      (when (funcall predicate obj)
        (push obj collection)))
    (nreverse collection)))

;;; Courtesy of Paul Graham...

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;; All code below courtesy of the PORT module, CLOCC project.

;;;
;;; Conditions
;;;

(define-condition code (error)
  ((proc :reader code-proc :initarg :proc)
   (mesg :type simple-string :reader code-mesg :initarg :mesg)
   (args :type list :reader code-args :initarg :args))
  (:documentation "An error in the user code.")
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s]~@[ ~?~]" (code-proc cc)
                     (and (slot-boundp cc 'mesg) (code-mesg cc))
                     (and (slot-boundp cc 'args) (code-args cc))))))

(define-condition case-error (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "`~s' evaluated to `~s', not one of [~@{`~s'~^ ~}]"))
  (:documentation "An error in a case statement.
This carries the function name which makes the error message more useful."))

(define-condition not-implemented (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "not implemented for ~a [~a]")
   (args :type list :reader code-args :initform
         (list (lisp-implementation-type) (lisp-implementation-version))))
  (:documentation "Your implementation does not support this functionality."))

;;;
;;; Extensions
;;;

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name)) (defun ,name ,arglist ,@body)))

(defmacro defcustom (name type init doc)
  "Define a typed global variable."
  `(progn (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defmacro defconst (name type init doc)
  "Define a typed constant."
  `(progn (declaim (type ,type ,name))
    ;; since constant redefinition must be the same under EQL, there
    ;; can be no constants other than symbols, numbers and characters
    ;; see ANSI CL spec 3.1.2.1.1.3 "Constant Variables"
    (,(if (subtypep type '(or symbol number character)) 'defconstant 'defvar)
     ,name (the ,type ,init) ,doc)))

(defmacro mk-arr (type init &optional len)
  "Make array with elements of TYPE, initializing."
  (if len `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
        :initial-contents ,init)))

(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))

(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (with-gensyms ("MI-" mi)
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

(defun gc ()
  "Invoke the garbage collector."
  #+allegro (excl:gc)
  #+clisp (#+lisp=cl ext:gc #-lisp=cl lisp:gc)
  #+cmu (ext:gc)
  #+cormanlisp (cl::gc)
  #+gcl (si::gbc)
  #+lispworks (hcl:normal-gc)
  #+lucid (lcl:gc)
  #+sbcl (sb-ext:gc)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'gc)))

(defun quit (&optional code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code)))

(defconst +eof+ cons (list '+eof+)
  "*The end-of-file object.
To be passed as the third arg to `read' and checked against using `eq'.")

(defun eof-p (stream)
  "Return T if the stream has no more data in it."
  (null (peek-char nil stream nil nil)))

(defun string-tokens (string &key (start 0) max)
  "Read from STRING repeatedly, starting with START, up to MAX tokens.
Return the list of objects read and the final index in STRING.
Binds `*package*' to the keyword package,
so that the bare symbols are read as keywords."
  (declare (type (or null fixnum) max) (type fixnum start))
  (let ((*package* (find-package :keyword)))
    (if max
        (do ((beg start) obj res (num 0 (1+ num)))
            ((= max num) (values (nreverse res) beg))
          (declare (fixnum beg num))
          (setf (values obj beg)
                (read-from-string string nil +eof+ :start beg))
          (if (eq obj +eof+)
              (return (values (nreverse res) beg))
              (push obj res)))
        (read-from-string (concatenate 'string "(" string ")")
                          t nil :start start))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (proclaim '(ftype (function () nil) required-argument))
    (defun required-argument ()
      "A useful default for required arguments and DEFSTRUCT slots."
      (error "A required argument was not supplied."))))

;;;
;;; Function Compositions
;;;

(defmacro compose (&rest functions)
  "Macro: compose functions or macros of 1 argument into a lambda.
E.g., (compose abs (dl-val zz) 'key) ==>
  (lambda (yy) (abs (funcall (dl-val zz) (funcall key yy))))"
  (labels ((rec (xx yy)
             (let ((rr (list (car xx) (if (cdr xx) (rec (cdr xx) yy) yy))))
               (if (consp (car xx))
                   (cons 'funcall (if (eq (caar xx) 'quote)
                                      (cons (cadar xx) (cdr rr)) rr))
                   rr))))
    (with-gensyms ("COMPOSE-" arg)
      (let ((ff (rec functions arg)))
        `(lambda (,arg) ,ff)))))

#|
(defun compose-f (&rest functions)
  "Return the composition of all the arguments.
All FUNCTIONS should take one argument, except for
the last one, which can take several."
  (reduce (lambda (f0 f1)
            (declare (function f0 f1))
            (lambda (&rest args) (funcall f0 (apply f1 args))))
          functions :initial-value #'identity))

(defun compose-all (&rest functions)
  "Return the composition of all the arguments.
All the values from nth function are fed to the n-1th."
  (reduce (lambda (f0 f1)
            (declare (function f0 f1))
            (lambda (&rest args) (multiple-value-call f0 (apply f1 args))))
          functions :initial-value #'identity))
|#
