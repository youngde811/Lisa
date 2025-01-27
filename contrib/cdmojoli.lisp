
(in-package :LISA-USER)

(defclass counter ()
  ((count :initform 0
          :reader counter-count)))

(defparameter *my-counter* (make-instance 'counter))

(assert-instance *my-counter*)

(defrule increment-when-zero ()
  (?counter (counter (count 0)))
  =>
  (modify ?counter (count 1)))

(format t "my-counter is at ~S~%" (counter-count *my-counter*))
(run)
(format t "my-counter is at ~S~%" (counter-count *my-counter*))
