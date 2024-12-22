
;;; This sample code was contributed by Gaston Pepe to isolate a bug he found with Lisa's
;;; TEST conditional element. The TEST CE must have been broken at least ten years ago via
;;; an unapproved commit, while Lisa was still hosted on SourceForge.

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
