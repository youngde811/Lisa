;;; This file is part of Lisa, the Lisp-based Intelligent Software Agents platform.

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

;; Description: User-customisable configuration settings for Lisa. It is expected
;; that developers will edit this file as they see fit.

(in-package :lisa)

;;; The reference guide has complete details, but:
;;;
;;; * Setting USE-FANCY-ASSERT enables the #? dispatch macro character.
;;; * Setting ALLOW-DUPLICATE-FACTS disables duplicate fact checking during
;;;   assertions.
;;; * Setting CONSIDER-TAXONOMY instructs Lisa to consider a CLOS instance's
;;;   ancestors during pattern matching.

(eval-when (:load-toplevel)
  (setf (use-fancy-assert) t)
  (setf (allow-duplicate-facts) t)
  (setf (consider-taxonomy) t))
