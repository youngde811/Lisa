(asdf:defsystem grouping-stack
  :name "grouping-stack"
  :version "0.0.1"
  :license "Artistic"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A stack implementation that allows automatic grouping and balancing of items for fast traversal."
  :homepage "https://github.com/Shinmera/SKEL"
  :serial T
  :components ((:file "package")
               (:file "item")
               (:file "stack")
               (:file "balancer"))
  :depends-on ())
