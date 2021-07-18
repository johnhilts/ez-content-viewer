(in-package #:content-viewer)

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun make-content-viewer-page ()
  "generate Content Viewer HTML page"
  (let ((file-list (list (list :f1 "one" :f2 "two") (list :f1 "three" :f2 "two") (list :f1 "one" :f2 "two"))))
  ;; (flet ((invoke-registered-ps-functions ()
  ;;    "pull all the registered ps functions from a global plist, then put them into a list"
  ;;    (do ((e *registered-ps-functions* (cddr e))
  ;;         (result ()))
  ;;        ((null e) result)
  ;;      (push (getf *registered-ps-functions* (car e)) result))))
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "EZ Content Viewer - File List")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css")
            (:script :type "text/javascript"
                     (str (eval (list 'ps (list 'var 'file-list (cons 'array (mapcar #'(lambda (e) `(create ,@e)) file-list))))))
                     (str (jfh-web:define-ps-with-html-macro))
                     ;; (str (share-server-side-constants))
                     ;; (str (client-todo))
                     ;; (str (client-app-settings))
                     ;; (str (client-ui))
                     ;; (dolist (e (invoke-registered-ps-functions))
                     ;;   (str (funcall e)))
                     ))
           (:body
            (:div :id "app-settings")
            (:div :id "file-type-filter"
                  (:div "Filter stuff here"))
            (:div
             (:h1 "File List"
                  (:div :class "main-container"
                        (:div :class "file-list"
                              (:div (:a :href "/media/test-17.jpg" "Test 17"))
                              (:div (:a :href "/media/test-18.jpg" "Test 18"))
                              (:div (:a :href "/media/test-19.jpg" "Test 19")))
                        (:div (:img :src "/media/test-17.jpg" :width 200 :height 200))))))))))
;)

(define-easy-handler (content-viewer-page :uri "/main") ()
  "HTTP endpoint for content-viewer page"
  (make-content-viewer-page))

(defun get-version ()
  "0.1")

(define-easy-handler (version-page :uri "/version") ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils / Content Viewer - Version")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css"))
     (:body
      (:div "Version")
      (:div (str (get-version)))))))
