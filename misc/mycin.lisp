;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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

;;; File: mycin.lisp
;;; Description: An implementation of MYCIN as illustrated in PAIP, pg. 553. The example
;;; is used to illustrate (and test) Lisa's new support for certainty factors. I didn't do
;;; a faithful port of the PAIP version; in particular, there's no interaction with the
;;; operator right now. However, all rules are present and the two scenarios on pgs. 555 and
;;; 556 are represented (by the functions CULTURE-1 and CULTURE-2).

;;; $Id: mycin.lisp,v 1.8 2006/04/14 16:49:32 youngde Exp $

(in-package :lisa-user)

(clear)

(setf lisa::*allow-duplicate-facts* nil)

(defclass param-mixin ()
  ((value :initarg :value
          :initform nil
          :reader value)
   (entity :initarg :entity
           :initform nil
           :reader entity)))

(defclass culture () ())

(defclass culture-site (param-mixin) ())

(defclass culture-age (param-mixin) ())

(defclass patient ()
  ((name :initarg :name
         :initform nil
         :reader name)
   (sex :initarg :sex
        :initform nil
        :reader sex)
   (age :initarg :age
        :initform nil
        :reader age)))

(defclass burn (param-mixin) ())

(defclass compromised-host (param-mixin) ())

(defclass organism () ())

(defclass gram (param-mixin) ())

(defclass morphology (param-mixin) ())

(defclass aerobicity (param-mixin) ())

(defclass growth-conformation (param-mixin) ())

(defclass organism-identity (param-mixin) ())

(defrule rule-52 (:belief 0.4)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (burn (value serious))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule rule-71 (:belief 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value clumps))
  =>
  (assert (organism-identity (value staphylococcus) (entity ?organism))))

(defrule rule-73 (:belief 0.9)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (aerobicity (value anaerobic))
  =>
  (assert (organism-identity (value bacteroides) (entity ?organism))))

(defrule rule-75 (:belief 0.6)
  (gram (value neg) (entity ?organism))
  (morphology (value rod))
  (compromised-host (value t))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule rule-107 (:belief 0.8)
  (gram (value neg) (organism ?organism))
  (morphology (value rod))
  (aerobicity (value aerobic))
  =>
  (assert (organism-identity (value enterobacteriaceae) (entity ?organism))))

(defrule rule-165 (:belief 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus))
  (growth-conformation (value chains))
  =>
  (assert (organism-identity (value streptococcus) (entity ?organism))))

(defrule conclusion (:salience -10)
  (?identity (organism-identity (value ?value)))
  =>
  (format t "Identity: ~A (~,3F)~%" ?value (belief:belief-factor ?identity)))

(defun culture-1 (&key (runp t))
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Sylvia Fischer"
                                 :sex 'female
                                 :age 27)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (burn (value serious) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (culture-age (value 3)))
    (assert (gram (value neg) (entity ?organism)))
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value aerobic) (entity ?organism)))
    (when runp
      (run))))

(defun culture-2 (&key (runp t))
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Sylvia Fischer"
                                 :sex 'female
                                 :age 27)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (burn (value serious) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (culture-age (value 3)))
    (assert (gram (value neg) (entity ?organism)) :belief 0.8)
    (assert (gram (value pos) (entity ?organism)) :belief 0.2)
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value anaerobic) (entity ?organism)))
    (when runp
      (run))))
