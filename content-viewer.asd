;;;; content-viewer.asd

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push #p"/home/jfh/code/lisp/source/util-lib/web/jfh-web/" asdf:*central-registry*)
(push #p"/home/jfh/code/lisp/source/web/content-viewer/" asdf:*central-registry*)
(asdf:load-system "jfh-web")
(compile-file #p"/home/jfh/code/lisp/source/util-lib/web/jfh-web/jfh-web.lisp")

(asdf:defsystem #:content-viewer
  :name "Content Viewer"
  :description "View files in a directory, with special handling for images and videos."
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-json #:swank #:drakma #:zpb-exif #:jfh-web)
  :components ((:file "package")
               (:file "common/constants")
               (:file "server/swank")
               (:file "server/web-common")
               (:file "server/util")
               (:file "server/io")
               (:file "server/file")
               (:file "server/api-file")
               (:file "server/web-infrastructure")
               (:file "client/common")
               (:file "client/ajax")
               (:file "client/util")
               (:file "client/file")
               (:file "client/ui")
               (:file "server/api")
               (:file "server/html")
               (:file "main")))

(defun buildapp ()
  (asdf:load-system :content-viewer)
  (save-lisp-and-die "content-viewer-app"
                     :toplevel 'cl-user::main
                     :executable t))
