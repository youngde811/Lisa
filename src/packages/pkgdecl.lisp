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

;; Description: Package declarations for Lisa.

(in-package :cl-user)

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (find-package 'clos))
             (find-package 'pcl))
    (rename-package (find-package 'pcl) 'pcl
                    `(clos ,@(package-nicknames 'pcl)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage "LISA"
    (:use "COMMON-LISP")
    (:export
      "ASSERT"
      "DEFAULT"
      .
      #1=(
          "*SHOW-LISA-WARNINGS*"
          "=>"
          "ACTIVATION"
          "ACTIVE-RULE"
          "AGENDA"
          "ALLOW-DUPLICATE-FACTS"
          "ASSERT-INSTANCE"
          "AUTO-FOCUS-P"
          "BINDINGS"
          "BREAKPOINTS"
          "CLEAR"
          "CLEAR-BREAK"
          "CLEAR-BREAKS"
          "CONSIDER-TAXONOMY"
          "CONTEXT"
          "CONTEXT-NAME"
          "CONTEXTS"
          "CURRENT-ENGINE"
          "DEFCONTEXT"
          "DEFFACTS"
          "DEFIMPORT"
          "DEFRULE"
          "DEFTEMPLATE"
          "DEPENDENCIES"
          "DUPLICATE-FACT"
          "ENGINE"
          "EXISTS"
          "FACT"
          "FACT-ID"
          "FACT-NAME"
          "FACTS"
          "FIND-CONTEXT"
          "FIND-FACT-BY-ID"
          "FIND-FACT-BY-NAME"
          "FIND-RULE"
          "FOCUS"
          "FOCUS-STACK"
          "HALT"
          "IN-RULE-FIRING-P"
          "INFERENCE-ENGINE"
          "INITIAL-FACT"
          "INSTANCE"
          "LOGICAL"
          "LOGGER-ADD-FILE-APPENDER"
          "MAKE-INFERENCE-ENGINE"
          "MARK-INSTANCE-AS-CHANGED"
          "MODIFY"
          "NEXT"
          "REFOCUS"
          "RESET"
          "RESUME"
          "RETE"
          "RETE-NETWORK"
          "RETRACT"
          "RETRACT-INSTANCE"
          "RETRIEVE"
          "RULE"
          "RULE-COMMENT"
          "RULE-CONTEXT"
          "RULE-DEFAULT-NAME"
          "RULE-NAME"
          "RULE-SALIENCE"
          "RULE-SHORT-NAME"
          "RULES"
          "RUN"
          "SET-BREAK"
          "SHOW-NETWORK"
          "SLOT"
          "SLOT-VALUE-OF-INSTANCE"
          "STANDARD-KB-CLASS"
          "TEST"
          "TOKEN"
          "TOKENS"
          "UNDEFCONTEXT"
          "UNDEFRULE"
          "UNWATCH"
          "USE-DEFAULT-ENGINE"
          "USE-FANCY-ASSERT"
          "USE-LISA"
          "WALK"
          "WATCH"
          "WATCHING"
          "WITH-INFERENCE-ENGINE"
          "WITH-SIMPLE-QUERY"))
    (:shadow "ASSERT"))

  (defpackage "LISA-USER"
    (:use "COMMON-LISP")
    (:shadowing-import-from "LISA" "ASSERT" "DEFAULT")
    (:import-from "LISA" . #1#)))

(defpackage "LISA.REFLECT"
  (:use "COMMON-LISP")
  (:nicknames "REFLECT")
  #+(or Allegro LispWorks)
  (:import-from "CLOS"
                "ENSURE-CLASS"
                "CLASS-DIRECT-SUPERCLASSES"
                "CLASS-FINALIZED-P"
                "FINALIZE-INHERITANCE")

  #+CMU
  (:import-from "CLOS"
                "CLASS-FINALIZED-P"
                "FINALIZE-INHERITANCE")
  #+:sbcl
  (:import-from "SB-MOP"
                "CLASS-FINALIZED-P"
                "FINALIZE-INHERITANCE")
  (:export
   "CLASS-ALL-SUPERCLASSES"
   "CLASS-FINALIZED-P"
   "CLASS-SLOT-LIST"
   "ENSURE-CLASS"
   "FINALIZE-INHERITANCE"
   "FIND-DIRECT-SUPERCLASSES"))

(defpackage "LISA.BELIEF"
  (:use "COMMON-LISP")
  (:nicknames "BELIEF")
  (:export
   "ADJUST-BELIEF"
   "BELIEF->ENGLISH"
   "BELIEF-FACTOR"
   "FALSE-P"
   "TRUE-P"
   "UKNOWN-P"))

(defpackage "LISA.HEAP"
  (:use "COMMON-LISP")
  (:nicknames "HEAP")
  (:export
   "CREATE-HEAP"
   "HEAP-CLEAR"
   "HEAP-COUNT"
   "HEAP-COLLECT"
   "HEAP-EMPTY-P"
   "HEAP-FIND"
   "HEAP-INSERT"
   "HEAP-PEEK"
   "HEAP-REMOVE"))
   
(defpackage "LISA.UTILS"
  (:use "COMMON-LISP")
  (:nicknames "UTILS")
  (:export
   "COLLECT"
   "COMPOSE"
   "COMPOSE-ALL"
   "COMPOSE-F"
   "FIND-AFTER"
   "FIND-BEFORE"
   "FIND-IF-AFTER"
   "FLATTEN"
   "LSTHASH"
   "MAP-IN"
   "STRING-TOKENS"))
