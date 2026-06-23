(in-package :lisa-bridge)

(defvar *bridge-port* 8090
  "Port on which the Lisa bridge HTTP server listens.")

(defvar *acceptor* nil
  "The active Hunchentoot acceptor, or NIL if the server is not running.")

(defun start (&key (port *bridge-port*))
  "Start the Lisa bridge HTTP server on PORT."
  (when *acceptor*
    (error "Lisa bridge is already running on port ~D." (hunchentoot:acceptor-port *acceptor*)))
  (setf *acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port
                       :name 'lisa-bridge))
  (hunchentoot:start *acceptor*)
  (format t "Lisa bridge started on port ~D.~%" port)
  *acceptor*)

(defun stop ()
  "Stop the Lisa bridge HTTP server."
  (unless *acceptor*
    (error "Lisa bridge is not running."))
  (hunchentoot:stop *acceptor*)
  (let ((port (hunchentoot:acceptor-port *acceptor*)))
    (setf *acceptor* nil)
    (format t "Lisa bridge stopped (was on port ~D).~%" port))
  t)