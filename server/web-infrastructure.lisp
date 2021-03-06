(in-package #:content-viewer)

(defun publish-static-content ()
  "static content"
  (push (create-static-file-dispatcher-and-handler
         "/favicon.ico" "ez-favicon.ico") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/media" "media") *dispatch-table*))

(defparameter *the-http-server* nil)

(defun stop-server (server)
  "Stop the server"
  (stop server))

(defun start-server (port)
  "start or re-start the web server; this gets called automatically when the server variable is declared"
  (setf *the-http-server*
        (restart-case (start (make-instance 'easy-acceptor :document-root #p"media" :port port))
          (re-start-server ()
            :report "Restart Web Server"
            (stop-server *the-http-server*)
            (start-server port)))))

(defun fetch-or-create-web-settings ()
  "read web-settings from persistence store; create default if doesn't exist yet"
  (let* ((default-web-settings  (list :web-port 80))
         (call-back #'(lambda (web-settings) (if web-settings web-settings default-web-settings))))
    (fetch-or-create-data *web-settings-file-path* call-back)))

(defun start-web-app ()
  "start the web app"
  ;; TODO - add code here to set the file paths if the configuration exists
   (setf *webroot-directory-path* (truename (make-pathname :name ".")))
  (ensure-directories-exist *share-root*)
  (start-server (getf (fetch-or-create-web-settings) :web-port))
  (publish-static-content))

