;;;; content-viewer.asd

(asdf:defsystem #:content-viewer
  :name "Content Viewer"
  :description "View files in a directory, with special handling for images and videos."
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-json #:swank #:drakma #:zpb-exif #:jfh-web)
  :components ((:file "package")
               (:file "content-viewer")))
