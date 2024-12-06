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

(in-package :lisa)

(defclass terminal-node ()
  ((rule :initarg :rule
         :initform nil
         :reader terminal-node-rule)))

(defmethod accept-token ((self terminal-node) (tokens add-token))
  (let* ((rule (terminal-node-rule self))
         (activation (make-activation rule tokens)))
    (add-activation (rule-engine rule) activation)
    (bind-rule-activation rule activation tokens)
    t))

(defmethod accept-token ((self terminal-node) (tokens remove-token))
  (let* ((rule (terminal-node-rule self))
         (activation (find-activation-binding rule tokens)))
    (unless (null activation)
      (disable-activation (rule-engine rule) activation)
      (unbind-rule-activation rule tokens))
    t))

(defmethod accept-token ((self terminal-node) (token reset-token))
  (clear-activation-bindings (terminal-node-rule self))
  t)

(defmethod print-object ((self terminal-node) strm)
  (print-unreadable-object (self strm :type t)
    (format strm "~A" (rule-name (terminal-node-rule self)))))

(defun make-terminal-node (rule)
  (make-instance 'terminal-node :rule rule))
