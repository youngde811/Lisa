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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package "LISA-HEATER"))
    (defpackage "LISA-HEATER"
      (:use "LISA-LISP")
      (:export "HEATER-SIMULATION"))))

(in-package "LISA-HEATER")

(make-inference-engine)

(deftemplate ambient-temperature ()
  (slot current-temperature)
  (slot desired-temperature)
  (slot location))

(deftemplate heater-state ()
  (slot state))

;; Rules

(defrule turn-on-heater ()
  (ambient-temperature (current-temperature ?ct) 
                       (desired-temperature ?dt (< ?ct ?dt))
                       (location ?loc))
  ; (test (equal ?loc "room"))
  =>
  (assert (heater-state (state on)))
  (print "Turning on the heater"))

(defrule turn-off-heater ()
  (ambient-temperature (current-temperature ?ct) 
                       (desired-temperature ?dt (>= ?ct ?dt))
                       (location ?loc))
  ; (test (equal ?loc "room"))
  =>
  (assert (heater-state (state off)))
  (print "Turning off the heater"))

;; Simulation

(defun heater-simulation ()
  ;; Initialize the system state
  (reset)
  (assert (ambient-temperature (current-temperature 20)
                                (desired-temperature 22)
                                (location "room")))
  (loop
    do
      ;; Execute the inference engine
      (run)

      ;; Add a new ambient-temperature fact
      (assert (ambient-temperature (current-temperature (+ 10 (random 20)))
                                    (desired-temperature 22)
                                    (location "room")))

      ;; Pause for 10 seconds
      (sleep 10)))
