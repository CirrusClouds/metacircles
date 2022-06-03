;;;; lisp-small.asd

(asdf:defsystem #:lisp-small
  :description "Describe lisp-small here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:alexandria #:serapeum)
  :components ((:file "package")
               (:file "lisp-small")
               (:file "tests")))
