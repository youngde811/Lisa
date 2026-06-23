(in-package :lisa-bridge)

(defun json-response (data &key (status 200))
  "Set response content type to JSON and return DATA serialized as a JSON string."
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (com.inuoe.jzon:stringify data))

(hunchentoot:define-easy-handler (health-check :uri "/health") ()
  (json-response
   (let ((ht (make-hash-table :test #'equal)))
     (setf (gethash "status" ht) "ok")
     ht)))