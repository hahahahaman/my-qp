;;;; my-qp.asd

(asdf:defsystem #:my-qp
  :description "Creates the skeleton of a new Common Lisp project"
  :depends-on (#:cl-fad
               #:html-template)
  :serial t
  :components ((:file "package")
               (:file "my-qp")))
