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

;; Description: Code that implements the Lisa programming language.

(in-package :lisa)

(defmacro defrule (name (&key (salience 0) (context nil) (belief nil) (auto-focus nil)) &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name ,@(if (consp name) `(,name) `(',name))))
       (redefine-defrule ,rule-name
                         ',body
                         :salience ,salience
                         :context ,context
                         :belief ,belief
                         :auto-focus ,auto-focus))))

(defun undefrule (rule-name)
  (with-rule-name-parts (context short-name long-name) rule-name
    (forget-rule (inference-engine) long-name)))

(defmacro deftemplate (name (&key) &body body)
  (redefine-deftemplate name body))

(defmacro defcontext (context-name &optional (strategy nil))
  `(unless (find-context (inference-engine) ,context-name nil)
     (register-new-context (inference-engine) 
                           (make-context ,context-name :strategy ,strategy))))

(defmacro undefcontext (context-name)
  `(forget-context (inference-engine) ,context-name))

(defun focus-stack ()
  (rete-focus-stack (inference-engine)))

(defun focus (&rest args)
  (if (null args)
      (current-context (inference-engine))
    (dolist (context-name (reverse args) (focus-stack))
      (push-context
       (inference-engine) 
       (find-context (inference-engine) context-name)))))

(defun refocus ()
  (pop-context (inference-engine)))

(defun contexts ()
  (let ((contexts (retrieve-contexts (inference-engine))))
    (dolist (context contexts)
      (format t "~S~%" context))
    (format t "For a total of ~D context~:P.~%" (length contexts))
    (values)))

(defun dependencies ()
  (maphash #'(lambda (dependent-fact dependencies)
               (format *trace-output* "~S:~%" dependent-fact)
               (format *trace-output* "  ~S~%" dependencies))
           (rete-dependency-table (inference-engine)))
  (values))

(defun expand-slots (body)
  (mapcar #'(lambda (pair)
              (destructuring-bind (name value) pair
                `(list (identity ',name) 
                       (identity 
                        ,@(if (quotablep value)
                              `(',value)
                            `(,value))))))
          body))

(defmacro assert ((name &body body) &key (belief nil))
  (let ((fact (gensym))
        (fact-object (gensym)))
    `(let ((,fact-object 
            ,@(if (or (consp name)
                      (variablep name))
                  `(,name)
                `(',name))))
       (if (typep ,fact-object 'standard-object)
           (parse-and-insert-instance ,fact-object :belief ,belief)
         (progn
           (ensure-meta-data-exists ',name)
           (let ((,fact (make-fact ',name ,@(expand-slots body))))
             (when (and (in-rule-firing-p)
                        (logical-rule-p (active-rule)))
               (bind-logical-dependencies ,fact))
             (assert-fact (inference-engine) ,fact :belief ,belief)))))))

(defmacro deffacts (name (&key &allow-other-keys) &body body)
  (parse-and-insert-deffacts name body))

(defun engine ()
  (active-engine))

(defun rule ()
  (active-rule))

(defun assert-instance (instance)
  (parse-and-insert-instance instance))

(defun retract-instance (instance)
  (parse-and-retract-instance instance (inference-engine)))

(defun facts ()
  (let ((facts (get-fact-list (inference-engine))))
    (dolist (fact facts)
      (format t "~S~%" fact))
    (format t "For a total of ~D fact~:P.~%" (length facts))
    (values)))

(defun rules (&optional (context-name nil))
  (let ((rules (get-rule-list (inference-engine) context-name)))
    (dolist (rule rules)
      (format t "~S~%" rule))
    (format t "For a total of ~D rule~:P.~%" (length rules))
    (values)))

(defun agenda (&optional (context-name nil))
  (let ((activations 
         (get-activation-list (inference-engine) context-name)))
    (dolist (activation activations)
      (format t "~S~%" activation))
    (format t "For a total of ~D activation~:P.~%" (length activations))
    (values)))

(defun reset ()
  (reset-engine (inference-engine)))

(defun clear ()
  (clear-system-environment))

(defun run (&optional (contexts nil))
  (unless (null contexts)
    (apply #'focus contexts))
  (run-engine (inference-engine)))

(defun walk (&optional (step 1))
  (run-engine (inference-engine) step))

(defmethod retract ((fact-object fact))
  (retract-fact (inference-engine) fact-object))

(defmethod retract ((fact-object number))
  (retract-fact (inference-engine) fact-object))

(defmethod retract ((fact-object t))
  (parse-and-retract-instance fact-object (inference-engine)))

(defmacro modify (fact &body body)
  `(modify-fact (inference-engine) ,fact ,@(expand-slots body)))

(defun watch (event)
  (watch-event event))

(defun unwatch (event)
  (unwatch-event event))

(defun watching ()
  (let ((watches (watches)))
    (format *trace-output* "Watching ~A~%"
            (if watches watches "nothing"))
    (values)))

(defun halt ()
  (halt-engine (inference-engine)))

(defun mark-instance-as-changed (instance &key (slot-id nil)) 
  (mark-clos-instance-as-changed (inference-engine) instance slot-id))
