(defun make-err-project (path)
  (my-qp:make-project
   path
   :depends-on '(:err)
   :template-directory "~/quicklisp/local-projects/my-qp/templates/err/"))

