(in-package :lisa-bridge)

(defun json-response (data &key (status 200))
  "Set response content type to JSON and return DATA serialized as a JSON string."
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (com.inuoe.jzon:stringify data))

(defun error-response (message &key (status 400))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "error" ht) message)
    (json-response ht :status status)))

(defun read-json-body ()
  "Read and parse the JSON request body."
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (when (and body (> (length body) 0))
      (com.inuoe.jzon:parse body))))

(hunchentoot:define-easy-handler (health-check :uri "/health") ()
  (json-response
   (let ((ht (make-hash-table :test #'equal)))
     (setf (gethash "status" ht) "ok")
     ht)))

(hunchentoot:define-easy-handler (assert-fact-handler :uri "/assert-fact"
                                                      :default-request-type :post) ()
  (let ((body (read-json-body)))
    (unless body
      (return-from assert-fact-handler
        (error-response "Request body must be valid JSON.")))
    (let ((fact-type (gethash "fact_type" body))
          (entity-name (gethash "entity" body))
          (value (gethash "value" body))
          (entity-class (or (gethash "entity_class" body) "organism"))
          (confidence (gethash "confidence" body)))
      (unless (and fact-type value)
        (return-from assert-fact-handler
          (error-response "fact_type and value are required.")))
      (handler-case
          (let* ((class-sym (find-symbol (string-upcase fact-type) :lisa-user))
                 (value-sym (intern (string-upcase value) :lisa-user))
                 (entity-obj (when entity-name
                               (find-or-create-entity entity-name entity-class)))
                 (initargs `(:value ,value-sym
                             ,@(when entity-obj
                                 `(:entity ,entity-obj))))
                 (instance (apply #'make-instance class-sym initargs)))
            (lisa:assert-instance instance :belief confidence)
            (let ((result (make-hash-table :test #'equal)))
              (setf (gethash "status" result) "asserted")
              (setf (gethash "fact_type" result) fact-type)
              (setf (gethash "value" result) value)
              (when entity-name
                (setf (gethash "entity" result) entity-name))
              (when confidence
                (setf (gethash "confidence" result) confidence))
              (json-response result)))
        (error (e)
          (error-response (format nil "Assertion failed: ~A" e) :status 500))))))

(hunchentoot:define-easy-handler (run-inference-handler :uri "/run-inference"
                                                        :default-request-type :post) ()
  (handler-case
      (let ((count (lisa:run)))
        (let ((result (make-hash-table :test #'equal)))
          (setf (gethash "status" result) "completed")
          (setf (gethash "rules_fired" result) count)
          (json-response result)))
    (error (e)
      (error-response (format nil "Inference failed: ~A" e) :status 500))))

(hunchentoot:define-easy-handler (conclusions-handler :uri "/conclusions"
                                                      :default-request-type :get) ()
  (handler-case
      (let ((facts (lisa:get-fact-list (lisa:inference-engine)))
            (conclusions '()))
        (dolist (fact facts)
          (when (eq (lisa:fact-name fact) 'lisa-user::organism-identity)
            (let ((entry (make-hash-table :test #'equal))
                  (val (lisa:get-slot-value fact (intern "VALUE" :lisa-user))))
              (setf (gethash "value" entry)
                    (string-downcase (symbol-name val)))
              (let ((belief (belief:belief-factor fact)))
                (when belief
                  (setf (gethash "belief" entry) belief)))
              (push entry conclusions))))
        (let ((result (make-hash-table :test #'equal)))
          (setf (gethash "conclusions" result) (coerce (nreverse conclusions) 'vector))
          (json-response result)))
    (error (e)
      (error-response (format nil "Failed to retrieve conclusions: ~A" e) :status 500))))

(hunchentoot:define-easy-handler (reset-handler :uri "/reset"
                                                :default-request-type :post) ()
  (handler-case
      (progn
        (reset-session)
        (let ((result (make-hash-table :test #'equal)))
          (setf (gethash "status" result) "reset")
          (json-response result)))
    (error (e)
      (error-response (format nil "Reset failed: ~A" e) :status 500))))