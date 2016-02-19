;;;; tests.lisp

(in-package #:(#| TMPL_VAR name |#).tests)

;;; "" goes here. Testing awaits!

(defun run-all-tests ())

;;; Hooking into ASDF
(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :(#| TMPL_VAR name |#).tests))))
  (format t "~2&*******************~@
                ** Starting test **~@
                *******************~%~%")
  (handler-bind ((style-warning #'muffle-warning)) (run-all-tests))
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~%"))
