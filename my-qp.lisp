;;;; my-qp.lisp

(in-package #:my-qp)

(defvar *name*)
(setf (documentation '*name* 'variable)
      "The name of the project currently being created.")

(defvar *template-directory* nil
  "A directory to use as a source of template files.")

(defvar *author*
  "Name <your@email.com>"
  "Set this variable to your contact information.")

(defvar *license*
  "Licenceless Rider")

(defvar *include-copyright* nil         ; This gives default behavior.
  "Include a copyright notice at the top of files.")

(defun uninterned-symbolize (name)
  "Return an uninterned symbol named after NAME, which is treated as a
string designator and upcased."
  (make-symbol (string-upcase name)))

(defun keyword-symbolize (name)
  (values (intern (string-upcase name) :keyword)))

(defun write-system-form (name
                          &key
                            depends-on
                            src-path
                            tests-path
                            (stream *standard-output*))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((*print-case* :downcase))
    ;; project
    (format stream "(asdf:defsystem ~S~%" (uninterned-symbolize name))
    (format stream "  :description \"Describe ~A here\"~%"
            name)
    (format stream "  :author ~S~%" *author*)
    (format stream "  :license ~S~%" *license*)
    (when depends-on
      (format stream "  :depends-on (~{~S~^~%~15T~})~%"
              (mapcar #'uninterned-symbolize depends-on)))
    (format stream "  :serial t~%")
    (format stream "  :pathname \"~A\"~%" src-path)
    (format stream "  :components ((:file \"package\")~%")
    (format stream "               (:file ~S)))~%"
            (string-downcase name))

    (format stream "~%")

    ;; test system
    (format stream "(asdf:defsystem ~S~%" (uninterned-symbolize
                                           (concatenate 'string
                                                        name
                                                        ".tests")))
    (format stream "  :description \"Describe ~A.tests here\"~%"
            name)
    (format stream "  :author ~S~%" *author*)
    (format stream "  :license ~S~%" *license*)
    (when depends-on
      (format stream "  :depends-on (~{~S~^~%~15T~})~%"
              (mapcar #'uninterned-symbolize
                      (append depends-on (list name "simple-testing")))))
    (format stream "  :serial t~%")
    (format stream "  :pathname \"~A\"~%" tests-path)
    (format stream "  :components ((:file \"package\")~%")
    (format stream "               (:file \"tests\")))~%")

    (format stream "~%")

    ;; asdf perform :test-op method so that tests can be run in one call
    (format stream "(defmethod asdf:perform ((op asdf:test-op)~%")
    (format stream "                         (system (eql (asdf:find-system ~S))))~%"
            (intern (string-upcase name) :keyword))
    (format stream "  (format t \"~A~%" "~2&*************~@")
    (format stream "~A~%" "                ** Loading **~@")
    (format stream "~A\")~%" "                *************~%")
    (format stream "  (asdf:oos 'asdf:load-op ~S)~%" (intern
                                                      (string-upcase
                                                       (concatenate 'string
                                                                    name
                                                                    ".tests"))
                                                      :keyword))
    (format stream "  (asdf:oos 'asdf:test-op ~S))" (intern
                                                     (string-upcase
                                                      (concatenate 'string
                                                                   name
                                                                   ".tests"))
                                                     :keyword))))

(defun pathname-project-name (pathname)
  "Return a project name based on PATHNAME by taking the last element
in the pathname-directory list. E.g. returns \"awesome-project\" for
#p\"src/awesome-project/\"."
  (first (last (pathname-directory pathname))))

(defmacro with-new-file ((stream file) &body body)
  "Like WITH-OPEN-FILE, but specialized for output to a file that must
not already exist."
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :error)
     (let ((*print-case* :downcase))
       ,@body)))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun file-comment-header (stream)
  (format stream ";;;; ~A~%" (file-namestring stream))
  (when *include-copyright*
    (format stream ";;;;~%")
    (format stream ";;;; Copyright (c) ~D ~A~%" (current-year) *author*))
  (terpri stream))

(defun write-system-file (name file &key depends-on src-path
                                      tests-path)
  (with-new-file (stream file)
    (file-comment-header stream)
    (write-system-form name
                       :depends-on depends-on
                       :stream stream
                       :src-path src-path
                       :tests-path tests-path)
    (terpri stream)))

(defun write-readme-file (name file)
  (with-new-file (stream file)
    (format stream "This is the stub ~A for the ~S project.~%"
            (file-namestring file)
            name)))

(defun write-package-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(defpackage ~S~%" (uninterned-symbolize name))
    (format stream "  (:use #:cl))~%~%")))

(defun write-application-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(in-package ~S)~%~%" (uninterned-symbolize name))
    (format stream ";;; ~S goes here. Hacks and glory await!~%~%" name)))

(defun write-test-package-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(defpackage ~S~%" (uninterned-symbolize (concatenate 'string
                                                                         name
                                                                         ".tests")))
    (format stream "  (:use #:cl ~S #:simple-testing)~%" (uninterned-symbolize name))
    (format stream "  (:export run-all-tests))")))

(defun write-test-application-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(in-package ~S)~%~%" (uninterned-symbolize
                                          (concatenate 'string
                                                       name
                                                       ".tests")))
    (format stream ";;; ~S goes here. Testing awaits!~%~%"
            (concatenate 'string
                         name
                         ".tests"))
    (format stream "(defun run-all-tests ())~%~%")

    (format stream "(defmethod asdf:perform ((op asdf:test-op)~%")
    (format stream "                         (system (eql (asdf:find-system ~S))))~%"
            (intern (string-upcase name) :keyword))
    (format stream "  (format t \"~A~%" "~2&*******************~@")
    (format stream "~A~%" "                ** Starting test **~@")
    (format stream "~A\")~%" "                *******************~%~%")
    (format stream "  (handler-bind ((style-warning #'muffle-warning)) (run-all-tests))~%")
    (format stream "~A~%~A~%~A~%"
            "  (format t \"~2&*****************************************~@"
            "                **            Tests finished           **~@"
            "                *****************************************~%\"))")))

(defvar *after-make-project-hooks* nil
  "A list of functions to call after MAKE-PROJECT is finished making a
project. Each function is called with the same arguments passed to
MAKE-PROJECT, except that NAME is canonicalized if
necessary. *DEFAULT-PATHNAME-DEFAULTS* bound to the newly created
project directory.")

(defun rewrite-templates (template-directory target-directory parameters)
  "Treat every file in TEMPLATE-DIRECTORY as a template file; fill it
out using PARAMETERS into a corresponding file in
TARGET-DIRECTORY. The rewriting uses HTML-TEMPLATE. The template start
marker is the string \"\(#|\" and the template end marker is the string
\"|#)\". Template vars are not modified or escaped when written."
  (let ((*template-start-marker* "(#|")
        (*template-end-marker* "|#)")
        (html-template:*warn-on-creation* nil)
        (html-template:*string-modifier* 'identity))
    (setf template-directory (truename template-directory)
          target-directory (truename target-directory))
    (flet ((rewrite-template (pathname)
             (let* ((relative-namestring
                     (enough-namestring pathname template-directory))
                    (target-pathname (merge-pathnames relative-namestring
                                                      target-directory)))
               (ensure-directories-exist target-pathname)
               (with-open-file (stream
                                target-pathname
                                :direction :output
                                :if-exists :rename-and-delete)
                 (fill-and-print-template pathname
                                          parameters
                                          :stream stream)))))
      (walk-directory template-directory #'rewrite-template))))

(defun default-template-parameters ()
  "Return a plist of :NAME, :LICENSE, and :AUTHOR parameters."
  (list :name *name*
        :license *license*
        :author *author*))

(defvar *template-parameter-functions* (list 'default-template-parameters)
  "A list of functions that return plists for use when rewriting
  template files. The results of calling each function are appended
  together to pass to FILL-AND-PRINT-TEMPLATE.")

(defun template-parameters (initial-parameters)
  "Return all template parameters returned by calling each element in
*TEMPLATE-PARAMETER-FUNCTIONS*, appended together as a single plist."
  (apply 'append initial-parameters
         (mapcar 'funcall *template-parameter-functions*)))

(defun make-project (pathname
                     &key
                       depends-on
                       template-parameters
                       ((:template-directory *template-directory*)
                        *template-directory*)
                       ((:author *author*) *author*)
                       ((:license *license*) *license*)
                       (name (pathname-project-name pathname) name-provided-p)
                       ((:include-copyright *include-copyright*) *include-copyright*)
                       (src-pathname "src/")
                       (tests-pathname "t/"))
  "Create a project s
(setf name (first (last (pathname-directory))))keleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (when (pathname-name pathname)
    (warn "Coercing ~S to directory"
          pathname)
    (setf pathname (pathname-as-directory pathname))
    (unless name-provided-p
      (setf name (pathname-project-name pathname))))
  (labels ((relative (file)
             (merge-pathnames file pathname))
           (src (file)
             (merge-pathnames file (relative src-pathname)))
           (tests (file)
             (merge-pathnames file (relative tests-pathname)))
           (nametype (type)
             (relative (make-pathname :name name :type type)))
           (src-nametype (type)
             (src (make-pathname :name name :type type))))
    (ensure-directories-exist pathname)
    (ensure-directories-exist (src ""))
    (ensure-directories-exist (tests ""))
    (write-readme-file name (relative "README.md"))
    (write-system-file name (nametype "asd") :depends-on depends-on
                                             :src-path src-pathname
                                             :tests-path tests-pathname)
    (write-package-file name (src "package.lisp"))
    (write-application-file name (src-nametype "lisp"))
    (write-test-package-file name (tests "package.lisp"))
    (write-test-application-file name (tests "tests.lisp"))

    (let ((*default-pathname-defaults* (truename pathname))
          (*name* name))
      (when *template-directory*
        (rewrite-templates *template-directory* *default-pathname-defaults*
                           (template-parameters template-parameters)))
      (pushnew *default-pathname-defaults* asdf:*central-registry*
               :test 'equal)
      (dolist (hook *after-make-project-hooks*)
        (funcall hook pathname :depends-on depends-on :name name
                               :allow-other-keys t)))
    name))


(defun make-err-project (path)
  (my-qp:make-project
   path
   :depends-on '(:err)
   :template-directory "~/quicklisp/local-projects/my-qp/templates/err/"))
