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

;; Description: An implementation of MYCIN as illustrated in PAIP, pg. 553. The example
;; is used to illustrate (and test) Lisa's new support for certainty factors. I didn't do
;; a faithful port of the PAIP version; in particular, there's no interaction with the
;; operator right now. However, all rules are present and the two scenarios on pgs. 555 and
;; 556 are represented (by the functions CULTURE-1 and CULTURE-2).

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
