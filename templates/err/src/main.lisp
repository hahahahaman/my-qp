(in-package #:(#| TMPL_VAR name |#))

(defun initialize ())
(defun handle-input ())
(defun render ())
(defun update ())
(defun cleanup ())

(defun run ()
  (err:run "insert-title"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
