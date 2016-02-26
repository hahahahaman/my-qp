(in-package #:(#| TMPL_VAR name |#))

(defglobal *project-directory* (asdf:system-source-directory :(#| TMPL_VAR name |#)))
(defglobal *shader-directory* (cl-fad:merge-pathnames-as-directory
                               *project-directory* (pathname "data/shaders/")))
(defglobal *texture-directory* (cl-fad:merge-pathnames-as-directory
                                *project-directory* (pathname "data/images/")))
(defglobal *font-directory* (cl-fad:merge-pathnames-as-directory
                             *project-directory* (pathname "data/fonts/")))
