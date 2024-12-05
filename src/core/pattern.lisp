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

;;; File: pattern.lisp
;;; Description: Structures here collectively represent patterns after they've
;;; been analysed by the language parser. This is the canonical representation
;;; of parsed patterns that Rete compilers are intended to see.

;;; $Id: pattern.lisp,v 1.2 2007/09/07 21:32:05 youngde Exp $

(in-package "LISA")

;;; Represents the canonical form of a slot within a pattern analysed by the
;;; DEFRULE parser. NAME is the slot identifier; VALUE is the slot's value,
;;; and its type can be one of (symbol number string list) or a LISA variable;
;;; SLOT-BINDING is the binding object, present if VALUE is a LISA variable;
;;; NEGATED is non-NIL if the slot occurs within a NOT form;
;;; INTRA-PATTERN-BINDINGS is a list of binding objects, present if all of the
;;; variables used by the slot reference bindings within the slot's pattern;
;;; CONSTRAINT, if not NIL, represents a constraint placed on the slot's
;;; value. CONSTRAINT should only be non-NIL if VALUE is a variable, and can
;;; be one of the types listed for VALUE or a CONS representing arbitrary
;;; Lisp code; CONSTRAINT-BINDINGS is a list of binding objects that are
;;; present if the slot has a constraint.

(defstruct pattern-slot
  "Represents the canonical form of a slot within a pattern analysed by the
  DEFRULE parser."
  (name nil :type symbol)
  (value nil)
  (slot-binding nil :type list)
  (negated nil :type symbol)
  (intra-pattern-bindings nil :type symbol)
  (constraint nil)
  (constraint-bindings nil :type list))

;;; PARSED-PATTERN represents the canonical form of a pattern analysed by the
;;; language parser. CLASS is the name, or head, of the pattern, as a symbol;
;;; SLOTS is a list of PATTERN-SLOT objects representing the analysed slots of
;;; the pattern; ADDRESS is a small integer representing the pattern's
;;; position within the rule form, starting at 0; PATTERN-BINDING, if not NIL,
;;; is the variable to which a fact matching the pattern will be bound during
;;; the match process; TEST-BINDINGS is a list of BINDING objects present if
;;; the pattern is a TEST CE; BINDING-SET is the set of variable bindings used
;;; by the pattern; TYPE is one of (:GENERIC :NEGATED :TEST :OR) and indicates
;;; the kind of pattern represented; SUB-PATTERNS, if non-NIL, is set for an
;;; OR CE and is a list of PARSED-PATTERN objects that represent the branches
;;; within the OR; LOGICAL, if non-NIL, indicates this pattern participates in
;;; truth maintenance.

(defstruct parsed-pattern
  "Represents the canonical form of a pattern analysed by the DEFRULE parser."
  (class nil :type symbol)
  (slots nil)
  (address 0 :type integer)
  (pattern-binding nil)
  (test-bindings nil :type list)
  (binding-set nil :type list)
  (logical nil :type symbol)
  (sub-patterns nil :type list)
  (type :generic :type symbol))

(defstruct rule-actions
  (bindings nil :type list)
  (actions nil :type list))

(defun generic-pattern-p (pattern)
  (eq (parsed-pattern-type pattern) :generic))

(defun existential-pattern-p (pattern)
  (eq (parsed-pattern-type pattern) :existential))

(defun test-pattern-p (pattern)
  (eq (parsed-pattern-type pattern) :test))

(defun test-pattern-predicate (pattern)
  (parsed-pattern-slots pattern))

(defun negated-pattern-p (pattern)
  (eq (parsed-pattern-type pattern) :negated))

(defun parsed-pattern-test-forms (pattern)
  (cl:assert (test-pattern-p pattern) nil
    "This pattern is not a test pattern: ~S" pattern)
  (parsed-pattern-slots pattern))

(defun simple-slot-p (pattern-slot)
  (not (variablep (pattern-slot-value pattern-slot))))

(defun intra-pattern-slot-p (pattern-slot)
  (or (simple-slot-p pattern-slot)
      (pattern-slot-intra-pattern-bindings pattern-slot)))

(defun constrained-slot-p (pattern-slot)
  (not (null (pattern-slot-constraint pattern-slot))))

(defun simple-bound-slot-p (pattern-slot)
  (and (variablep (pattern-slot-value pattern-slot))
       (not (constrained-slot-p pattern-slot))))

(defun negated-slot-p (pattern-slot)
  (pattern-slot-negated pattern-slot))

(defun bound-pattern-p (parsed-pattern)
  (not (null (parsed-pattern-pattern-binding parsed-pattern))))

(defun compound-pattern-p (parsed-pattern)
  (not (null (parsed-pattern-sub-patterns parsed-pattern))))

(defun logical-pattern-p (parsed-pattern)
  (parsed-pattern-logical parsed-pattern))
