(in-package #:cl-user)
(defpackage #:grouping-stack
  (:nicknames #:org.shirakumo.grouping-stack)
  (:use #:cl)
  ;; balancer.lisp
  (:export
   #:balancer
   #:inactive-balancer
   #:sink-balancer
   #:buffer-size)
  ;; item.lisp
  (:export
   #:combinable-p
   #:combine
   #:item
   #:content
   #:make-item
   #:group
   #:items)
  ;; stack.lisp
  (:export
   #:grouping-stack
   #:stack-items
   #:stack-count
   #:stack-balancer
   #:balance
   #:make-grouping-stack
   #:stack-push
   #:stack-push-many
   #:stack-pop
   #:stack-size
   #:stack-clear
   #:map-stack))
