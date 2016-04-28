(in-package #:(#| TMPL_VAR name |#))

(defun initialize ())
(defun handle-input ()
  (when (key-pressed-p :escape)
    (close-window)))

(defrender render +max-fps+
  ;; render code
  )

(defupdate update +max-fps+
  ;; update code
  )

(defun cleanup ())

(defun run ()
  (err-run "insert-title"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
