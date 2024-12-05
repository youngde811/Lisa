;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: pkgdecl.lisp
;;; Description: Package declarations for LISA.

;;; $Id: pkgdecl.lisp,v 1.81 2007/09/08 14:48:59 youngde Exp $

(in-package "CL-USER")

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
