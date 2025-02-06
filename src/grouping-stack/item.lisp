(in-package #:org.shirakumo.grouping-stack)

(defgeneric combinable-p (a b)
  (:method (a b)
    (declare (ignore a b))
    T))

(defgeneric combine (content &rest other-content))

(defclass item ()
  ((content :initarg :content :initform (error "CONTENT required.") :accessor content)))

(defmethod print-object ((item item) stream)
  (print-unreadable-object (item stream :type T)
    (write (content item) :stream stream))
  item)

(defun make-item (content)
  (make-instance 'item :content content))

(defclass group (item)
  ((items :initarg :items :initform () :accessor items)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type T)
    (write (content group) :stream stream)
    (write (items group)))
  group)
