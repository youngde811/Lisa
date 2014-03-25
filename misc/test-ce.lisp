
(in-package :lisa-user)

(clear)

(defclass frodo ()
  ((name :initarg :name :initform nil :accessor name)))

(defrule frodo ()
  (frodo (name ?name))
  (test (equal ?name "frodo"))
  =>
  (format t "frodo fired; name is ~A~%" ?name))

(assert (frodo (name "frodo")))
(assert (frodo (name "bilbo")))
