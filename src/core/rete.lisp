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

;; Description: Class representing the inference engine itself.

(in-package :lisa)

(defclass rete ()
  ((fact-table :initform (make-hash-table :test #'equalp)
               :accessor rete-fact-table)
   (fact-id-table :initform (make-hash-table)
                  :accessor fact-id-table)
   (instance-table :initform (make-hash-table)
                   :reader rete-instance-table)
   (rete-network :initform (make-rete-network)
                 :reader rete-network)
   (next-fact-id :initform -1
                 :accessor rete-next-fact-id)
   (autofacts :initform (list)
              :accessor rete-autofacts)
   (meta-data :initform (make-hash-table)
              :reader rete-meta-data)
   (dependency-table :initform (make-hash-table :test #'equal)
                     :accessor rete-dependency-table)
   (contexts :initform (make-hash-table :test #'equal)
             :reader rete-contexts)
   (focus-stack :initform (list)
                :accessor rete-focus-stack)
   (halted :initform nil
           :accessor rete-halted)
   (firing-count :initform 0
                 :accessor rete-firing-count)))

(defmethod initialize-instance :after ((self rete) &rest initargs)
  (declare (ignore initargs))
  (register-new-context self (make-context :initial-context))
  (reset-focus-stack self)
  self)

;;; FACT-META-OBJECT represents data about facts. Every Lisa fact is backed by
;;; a CLOS instance that was either defined by the application or internally
;;; by Lisa (via DEFTEMPLATE).

(defstruct fact-meta-object
  (class-name nil :type symbol)
  (slot-list nil :type list)
  (superclasses nil :type list))

(defun register-meta-object (rete key meta-object)
  (setf (gethash key (rete-meta-data rete)) meta-object))

(defun find-meta-object (rete symbolic-name)
  (gethash symbolic-name (rete-meta-data rete)))

(defun rete-fact-count (rete)
  (hash-table-count (rete-fact-table rete)))

(defun find-rule (rete rule-name)
  (with-rule-name-parts (context-name short-name long-name) rule-name
    (find-rule-in-context (find-context rete context-name) long-name)))

(defun add-rule-to-network (rete rule patterns)
  (flet ((load-facts (network)
           (maphash #'(lambda (key fact)
                        (declare (ignore key))
                        (add-fact-to-network network fact))
                    (rete-fact-table rete))))
    (when (find-rule rete (rule-name rule))
      (forget-rule rete rule))
    (if (zerop (rete-fact-count rete))
        (compile-rule-into-network (rete-network rete) patterns rule)
      (merge-rule-into-network 
       (rete-network rete) patterns rule :loader #'load-facts))
    (add-rule-to-context (rule-context rule) rule)
    rule))

(defmethod forget-rule ((self rete) (rule-name symbol))
  (flet ((disable-activations (rule)
           (mapc #'(lambda (activation)
                     (setf (activation-eligible activation) nil))
                 (find-all-activations
                  (context-strategy (rule-context rule)) rule))))
    (let ((rule (find-rule self rule-name)))
      (cl:assert (not (null rule)) nil
        "The rule named ~S is not known to be defined." rule-name)
      (remove-rule-from-network (rete-network self) rule)
      (remove-rule-from-context (rule-context rule) rule)
      (disable-activations rule)
      rule)))

(defmethod forget-rule ((self rete) (rule rule))
  (forget-rule self (rule-name rule)))

(defmethod forget-rule ((self rete) (rule-name string))
  (forget-rule self (find-symbol rule-name)))

(defun remember-fact (rete fact)
  (with-accessors ((fact-table rete-fact-table)
                   (id-table fact-id-table)) rete
    (setf (gethash (hash-key fact) fact-table) fact)
    (setf (gethash (fact-id fact) id-table) fact)))

(defun forget-fact (rete fact)
  (with-accessors ((fact-table rete-fact-table)
                   (id-table fact-id-table)) rete
    (remhash (hash-key fact) fact-table)
    (remhash (fact-id fact) id-table)))

(defun find-fact-by-id (rete fact-id)
  (gethash fact-id (fact-id-table rete)))

(defun find-fact-by-name (rete fact-name)
  (gethash fact-name (rete-fact-table rete)))

(defun forget-all-facts (rete)
  (clrhash (rete-fact-table rete))
  (clrhash (fact-id-table rete)))

(defun get-fact-list (rete)
  (delete-duplicates
   (sort
    (loop for fact being the hash-values of (rete-fact-table rete)
        collect fact)
    #'(lambda (f1 f2) (< (fact-id f1) (fact-id f2))))))

(defun duplicate-fact-p (rete fact)
  (let ((f (gethash (hash-key fact) (rete-fact-table rete))))
    (if (and f (equals f fact))
        f
      nil)))

(defmacro ensure-fact-is-unique (rete fact)
  (let ((existing-fact (gensym)))
    `(unless *allow-duplicate-facts*
       (let ((,existing-fact
              (gethash (hash-key ,fact) (rete-fact-table ,rete))))
         (unless (or (null ,existing-fact)
                     (not (equals ,fact ,existing-fact)))
           (error (make-condition 'duplicate-fact :existing-fact ,existing-fact)))))))
  
(defmacro with-unique-fact ((rete fact) &body body)
  (let ((body-fn (gensym))
        (existing-fact (gensym)))
    `(flet ((,body-fn ()
              ,@body))
       (if *allow-duplicate-facts*
           (,body-fn)
         (let ((,existing-fact (duplicate-fact-p ,rete ,fact)))
           (if (not ,existing-fact)
               (,body-fn)
             (error (make-condition 'duplicate-fact
                                    :existing-fact ,existing-fact))))))))
  
(defun next-fact-id (rete)
  (incf (rete-next-fact-id rete)))

(defun add-autofact (rete deffact)
  (pushnew deffact (rete-autofacts rete) :key #'deffacts-name))

(defun remove-autofacts (rete)
  (setf (rete-autofacts rete) nil))

(defun assert-autofacts (rete)
  (mapc #'(lambda (deffact)
            (mapc #'(lambda (fact)
                      (assert-fact rete (make-fact-from-template fact)))
                  (deffacts-fact-list deffact)))
        (rete-autofacts rete)))

(defmethod assert-fact-aux ((self rete) fact)
  (with-truth-maintenance (self)
    (setf (fact-id fact) (next-fact-id self))
    (remember-fact self fact)
    (trace-assert fact)
    (add-fact-to-network (rete-network self) fact)
    (when (fact-shadowsp fact)
      (register-clos-instance self (find-instance-of-fact fact) fact)))
  fact)
  
(defmethod adjust-belief (rete fact (belief-factor number))
   (with-unique-fact (rete fact)
     (setf (belief-factor fact) belief-factor)))

(defmethod adjust-belief (rete fact (belief-factor t))
  (declare (ignore rete))
  (when (in-rule-firing-p)
    (let ((rule-belief (belief-factor (active-rule)))
          (facts (token-make-fact-list *active-tokens*)))
      (setf (belief-factor fact) (belief:adjust-belief facts rule-belief (belief-factor fact))))))

(defmethod assert-fact ((self rete) fact &key belief)
  (let ((duplicate (duplicate-fact-p self fact)))
    (cond (duplicate
           (adjust-belief self duplicate belief))
          (t
           (adjust-belief self fact belief)
           (assert-fact-aux self fact)))
    (if duplicate duplicate fact)))

(defmethod retract-fact ((self rete) (fact fact))
  (with-truth-maintenance (self)
    (forget-fact self fact)
    (trace-retract fact)
    (remove-fact-from-network (rete-network self) fact)
    (when (fact-shadowsp fact)
      (forget-clos-instance self (find-instance-of-fact fact)))
    fact))

(defmethod retract-fact ((self rete) (instance standard-object))
  (let ((fact (find-fact-using-instance self instance)))
    (cl:assert (not (null fact)) nil
      "This CLOS instance is unknown to LISA: ~S" instance)
    (retract-fact self fact)))

(defmethod retract-fact ((self rete) (fact-id integer))
  (let ((fact (find-fact-by-id self fact-id)))
    (and (not (null fact))
         (retract-fact self fact))))

(defmethod modify-fact ((self rete) fact &rest slot-changes)
  (retract-fact self fact)
  (mapc #'(lambda (slot)
            (set-slot-value fact (first slot) (second slot)))
        slot-changes)
  (assert-fact self fact)
  fact)

(defun clear-contexts (rete)
  (loop for context being the hash-values of (rete-contexts rete)
      do (clear-activations context)))

(defun clear-focus-stack (rete)
  (setf (rete-focus-stack rete) (list)))

(defun initial-context (rete)
  (find-context rete :initial-context))

(defun reset-focus-stack (rete)
  (setf (rete-focus-stack rete)
    (list (initial-context rete))))

(defun set-initial-state (rete)
  (forget-all-facts rete)
  (clear-contexts rete)
  (reset-focus-stack rete)
  (setf (rete-next-fact-id rete) -1)
  (setf (rete-firing-count rete) 0)
  t)

(defmethod reset-engine ((self rete))
  (reset-network (rete-network self))
  (set-initial-state self)
  (assert (initial-fact))
  (assert-autofacts self)
  t)

(defun get-rule-list (rete &optional (context-name nil))
  (if (null context-name)
      (loop for context being the hash-values of (rete-contexts rete)
          append (context-rule-list context))
    (context-rule-list (find-context rete context-name))))

(defun get-activation-list (rete &optional (context-name nil))
  (if (not context-name)
      (loop for context being the hash-values of (rete-contexts rete)
            for activations = (context-activation-list context)
            when activations
              nconc activations)
    (context-activation-list (find-context rete context-name))))

(defun find-fact-using-instance (rete instance)
  (gethash instance (rete-instance-table rete)))

(defun register-clos-instance (rete instance fact)
  (setf (gethash instance (rete-instance-table rete)) fact))

(defun forget-clos-instance (rete instance)
  (remhash instance (rete-instance-table rete)))

(defun forget-clos-instances (rete)
  (clrhash (rete-instance-table rete)))

(defmethod mark-clos-instance-as-changed ((self rete) instance
                                          &optional (slot-id nil))
  (let ((fact (find-fact-using-instance self instance))
        (network (rete-network self)))
    (cond ((null fact)
           (warn "This instance is not known to Lisa: ~S." instance))
          (t
           (remove-fact-from-network network fact)
           (synchronize-with-instance fact slot-id)
           (add-fact-to-network network fact)))
    instance))

(defun find-context (rete defined-name &optional (errorp t))
  (let ((context
         (gethash (make-context-name defined-name) (rete-contexts rete))))
    (if (and (null context) errorp)
        (error "There's no context named: ~A" defined-name)
      context)))

(defun register-new-context (rete context)
  (setf (gethash (context-name context) (rete-contexts rete)) context))

(defun forget-context (rete context-name)
  (let ((context (find-context rete context-name)))
    (dolist (rule (context-rule-list context))
      (forget-rule rete rule))
    (remhash context-name (rete-contexts rete))
    context))

(defun current-context (rete)
  (first (rete-focus-stack rete)))

(defun next-context (rete)
  (with-accessors ((focus-stack rete-focus-stack)) rete
    (pop focus-stack)
    (setf *active-context* (first focus-stack))))

(defun starting-context (rete)
  (first (rete-focus-stack rete)))

(defun push-context (rete context)
  (push context (rete-focus-stack rete))
  (setf *active-context* context))

(defun pop-context (rete)
  (next-context rete))

(defun retrieve-contexts (rete)
  (loop for context being the hash-values of (rete-contexts rete)
      collect context))

(defmethod add-activation ((self rete) activation)
  (let ((rule (activation-rule activation)))
    (trace-enable-activation activation)
    (add-activation (conflict-set rule) activation)
    (when (auto-focus-p rule)
      (push-context self (rule-context rule)))))

(defmethod disable-activation ((self rete) activation)
  (when (eligible-p activation)
    (trace-disable-activation activation)
    (setf (activation-eligible activation) nil))
  activation)

(defmethod run-engine ((self rete) &optional (step -1))
  (with-context (starting-context self)
    (setf (rete-halted self) nil)
    (do ((count 0))
        ((or (= count step) (rete-halted self)) count)
      (let ((activation 
             (next-activation (conflict-set (active-context)))))
        (cond ((null activation)
               (next-context self)
               (when (null (active-context))
                 (reset-focus-stack self)
                 (halt-engine self)))
              ((eligible-p activation)
               (incf (rete-firing-count self))
               (fire-activation activation)
               (incf count)))))))

(defun halt-engine (rete)
  (setf (rete-halted rete) t))

(defun make-rete ()
  (make-instance 'rete))

(defun make-inference-engine ()
  (make-rete))

(defun copy-network (engine)
  (let ((new-engine (make-inference-engine)))
    (mapc #'(lambda (rule)
              (copy-rule rule new-engine))
          (get-rule-list engine))
    new-engine))

(defun make-query-engine (source-rete)
  (let* ((query-engine (make-inference-engine)))
    (loop for fact being the hash-values of (rete-fact-table source-rete)
        do (remember-fact query-engine fact))
    query-engine))
