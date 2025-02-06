(in-package #:org.shirakumo.grouping-stack)

(defclass balancer ()
  ())

(defclass inactive-balancer (balancer)
  ())

(defmethod balance ((stack grouping-stack) (balancer inactive-balancer))
  (declare (ignore balancer))
  stack)

(defclass sink-balancer (balancer)
  ((buffer-size :initarg :buffer-size :initform 2 :accessor buffer-size)))

(defmethod balance ((stack grouping-stack) (balancer sink-balancer))
  (let ((items (nthcdr (buffer-size balancer) (stack-items stack))))
    (unless (null (cdr items))
      (setf (car items) (make-instance 'item :content (apply #'combine (mapcar #'content items)))
            (cdr items) NIL))))

(defclass grouping-balancer (balancer)
  ((grouping-threshold :initarg :grouping-threshold :initform 2 :accessor grouping-threshold)
   (group-size :initarg :group-size :initform 2 :accessor group-size)
   (max-items :initarg :max-items :initform NIL :accessor max-items)))

(defmethod balance ((stack grouping-stack) (balancer grouping-balancer))
  )
