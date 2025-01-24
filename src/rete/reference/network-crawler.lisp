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

(defun show-network (&optional (rete-network (active-network)) (strm *terminal-io*))
  (labels ((get-roots ()
             (loop for node being the hash-values of (rete-roots rete-network)
                 collect node))
           (get-successors (shared-node)
             (loop for s being the hash-values of (shared-node-successors shared-node) 
                 collect (successor-node s)))
           (get-successor (join-node)
             (list (successor-node (join-node-successor join-node))))
           (trace-nodes (nodes &optional (level 0))
             (unless (null nodes)
               (let* ((node (first nodes))
                      (string (format nil "~S" node)))
                 (format strm "~V<~A~>~%" (+ level (length string)) string)
                 (typecase node
                   (shared-node
                    (trace-nodes (get-successors node) (+ level 3)))
                   (join-node
                    (trace-nodes (get-successor node) (+ level 3)))
                   (terminal-node
                    nil))
                 (trace-nodes (rest nodes) level)))))
    (trace-nodes (get-roots))))
