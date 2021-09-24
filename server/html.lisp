(in-package #:content-viewer)

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun make-content-viewer-page ()
  "generate Content Viewer HTML page"
  (let* ((folder-index (parse-integer (or (parameter "fi" *request*) "0")))
         (file-list (get-file-list (nth folder-index *folders*)))
         (image-list (remove-if #'(lambda (e)  (file-signaled-error e)) (content-images file-list)))
         (video-list (content-videos file-list))
         (folder-list (content-folders file-list))
         (favorite-name-list (get-favorite-name-list)))
    (setf *folders* (index-folders file-list *folders*)) ;; this needs to survive across requests
    (flet ((invoke-registered-ps-functions ()
             "pull all the registered ps functions from a global plist, then put them into a list"
             (do ((e *registered-ps-functions* (cddr e))
                  (result ()))
                 ((null e) result)
               (push (getf *registered-ps-functions* (car e)) result)))
           (to-javascript-array (file-list-name file-list)
             (let ((aliased-folder-list (index-alias-folders *folders*)))
               (eval (list
                      'ps
                      (list 'var file-list-name
                            (cons 'array
                                  (mapcar #'(lambda (e) (create-javascript-object e aliased-folder-list)) file-list))))))))
      (with-html-output-to-string
          (*standard-output* nil :prologue t :indent t)
        (:html :lang "en"
               (:head
                (:meta :charset "utf-8")
                (:title "EZ Content Viewer - File List")
                (:link :type "text/css"
                       :rel "stylesheet"
                       :href (str (format nil "/styles.css?v=~a" (get-version))))
                (:script :type "text/javascript"
                         (str (to-javascript-array 'image-list image-list))
                         (str (to-javascript-array 'video-list video-list))
                         (str (to-javascript-array 'folder-list folder-list))
                         (str (to-javascript-array 'favorite-name-list favorite-name-list))
                         (str (jfh-web:define-ps-with-html-macro))
                         (str (share-server-side-constants))
                         (str (client-favorite))
                         ;; (str (client-app-settings))
                         (str (client-ui))
                         (dolist (e (invoke-registered-ps-functions))
                           (str (funcall e)))))
               (:body
                (:div :id "file-list" :class "row" :hidden "false"
                      (:div :id "left" :class "column"
                            (:div :class "top-left"
                                  (:div
                                   (:a :href "/main" "Home"))
                                  (let ((previous-index (get-previous-folder-index folder-index *folders*)))
                                    (when previous-index
                                      (htm
                                       (:div
                                        (:a :href (format nil "/main?fi=~a" previous-index) "[ Previous ]"))))))
                            (:div :class "bottom" :id "left-bottom"))
                      (:div :id "right" :class "column"
                            (:div :class "top-right" :id "top-right") ;; let's do the markup in the client so that a post-back isn't necessary
                            (:div :class "bottom" :id "right-bottom")))
                (:div :id "full-size-parent" :hidden "true")))))))

(define-easy-handler (content-viewer-page :uri "/main") ()
  "HTTP endpoint for content-viewer page"
  (make-content-viewer-page))

(defun get-version ()
  "0.24")

(define-easy-handler (version-page :uri "/version") ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils / Content Viewer - Version")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href (format nil "/styles.css?v=~a" (get-version))))
     (:body
      (:div "Version")
      (:div (str (get-version)))))))
