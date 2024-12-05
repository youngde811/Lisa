;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: terminal-node.lisp
;;; Description:

;;; $Id: terminal-node.lisp,v 1.12 2004/09/13 19:27:53 youngde Exp $

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
