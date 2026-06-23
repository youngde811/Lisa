(in-package :lisa-bridge)

(defvar *session-entities* (make-hash-table :test #'equal)
  "Maps entity name strings (e.g. \"organism-1\") to CLOS instances.")

(defvar *session-lock* (bt:make-lock "lisa-bridge-session")
  "Lock protecting session state.")

(defun reset-session ()
  "Clear all session state and reset the Lisa engine."
  (bt:with-lock-held (*session-lock*)
    (clrhash *session-entities*)
    (lisa:reset)))

(defun find-or-create-entity (entity-name class-name)
  "Look up an entity by name, creating it if it doesn't exist.
   CLASS-NAME is a string like \"organism\" or \"patient\"."
  (bt:with-lock-held (*session-lock*)
    (or (gethash entity-name *session-entities*)
        (let* ((class-sym (find-symbol (string-upcase class-name) :lisa-user))
               (instance (make-instance class-sym)))
          (setf (gethash entity-name *session-entities*) instance)
          instance))))

(defun get-entity (entity-name)
  "Look up an existing entity by name. Returns NIL if not found."
  (bt:with-lock-held (*session-lock*)
    (gethash entity-name *session-entities*)))