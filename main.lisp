;;;; web app entry point

(in-package #:content-viewer)

(import-macros-from-lisp 'with-html-elements)


(in-package #:cl-user)

(defun main ()
  (content-viewer::start-web-app)
  (sb-impl::toplevel-repl nil))

