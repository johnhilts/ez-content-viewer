(in-package #:content-viewer)

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun make-content-viewer-page ()
  "generate Content Viewer HTML page"
  (let* ((init-file-list (make-list 1000 :initial-element '("media/photo/test" . 1)))
         (index 0)
         (file-list (mapcar #'(lambda (e)
                                (incf index)
                                (cons (car e) index)) init-file-list)))
    (flet ((invoke-registered-ps-functions ()
             "pull all the registered ps functions from a global plist, then put them into a list"
             (do ((e *registered-ps-functions* (cddr e))
                  (result ()))
                 ((null e) result)
               (push (getf *registered-ps-functions* (car e)) result))))
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
                         (str (eval (list 'ps (list 'var 'file-list (cons 'array (mapcar #'(lambda (e) `(create :path ,(format nil "~a~a.jpg" (car e) (cdr e)))) file-list))))))
                         (str (jfh-web:define-ps-with-html-macro))
                         (str (share-server-side-constants))
                         ;; (str (client-todo))
                         ;; (str (client-app-settings))
                         (str (client-ui))
                         (dolist (e (invoke-registered-ps-functions))
                           (str (funcall e)))))
               (:body
                (:div :id "left" :class "column"
                      (:div :class "top-left"  "top left")
                      (:div :class "bottom"
                            (dolist (file file-list)
a                              (htm (:div :class "column-item" (:a :href (format nil "~a~a.jpg Test" (car file) (cdr file)) (str (format nil "Test~a.jpg" (cdr file)))))))
                            (:div (:a :href "/media/test-17.jpg" "Test 17"))
                            (:div (:a :href "/media/test-18.jpg" "Test 18"))
                            (:div (:a :href "/media/test-19.jpg" "Test 19"))))
                (:div :id "right" :class "column"
                      (:div :class "top-right" "top right")
                      (:div :class "bottom"
                            (:img :src "/media/photos/TestImage.JPG" :style (str (format nil "transform: rotate(~ddeg)" (- 360 270))) :width 200 :height 200)))))))))

(defun make-content-viewer-page-use-js ()
  "generate Content Viewer HTML page"
  (let* ((folder-index (parse-integer (or (parameter "fi" *request*) "0")))
         (file-list (get-file-list (nth folder-index *folders*)))
         (image-list (content-images file-list))
         (video-list (content-videos file-list))
         (folder-list (content-folders file-list)))
    (setf *folders* (index-folders file-list *folders*)) ;; this needs to survive across requests
    (labels ((get-web-path (file-path)
               (let* ((path (namestring file-path))
                      (web-path-start (search (subseq *content-root* 1) path)))
                 (if web-path-start
                     (subseq path web-path-start)
                     path))))
      (flet ((invoke-registered-ps-functions ()
               "pull all the registered ps functions from a global plist, then put them into a list"
               (do ((e *registered-ps-functions* (cddr e))
                    (result ()))
                   ((null e) result)
                 (push (getf *registered-ps-functions* (car e)) result)))
             (to-javascript-array (file-list-name file-list)
               (eval (list
                      'ps
                      (list 'var file-list-name
                            (cons 'array
                                  (mapcar #'(lambda (e)
                                              (let* ((file-path (file-path e))
                                                     (file-content-type (file-content-type e))
                                                     (folder-index (cond
                                                                     ((and (equal 'folder file-content-type) (equal *content-root* file-path)) 0)
                                                                     ((equal 'folder file-content-type) (+ 1 (search-folders file-path (cdr *folders*))))
                                                                     (t -1))))
                                                `(create
                                                  :path ,(get-web-path file-path)
                                                  ,(symbol-to-js-string :content-type) ,(symbol-to-js-string file-content-type)
                                                  ,(symbol-to-js-string :folder-index) ,folder-index)))
                                          file-list)))))))
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
                           (str (jfh-web:define-ps-with-html-macro))
                           (str (share-server-side-constants))
                           ;; (str (client-todo))
                           ;; (str (client-app-settings))
                           (str (client-ui))
                           (dolist (e (invoke-registered-ps-functions))
                             (str (funcall e)))))
                 (:body
                  (:div :id "file-list" :class "row"
                        (:div :id "left" :class "column"
                              (:div :class "top-left"  "top left"
                                    (let ((previous-index (get-previous-folder-index folder-index *folders*)))
                                      (when previous-index
                                        (htm
                                         (:div
                                          (:a :href (format nil "/main-js?fi=~a" previous-index) "[ Previous ]"))))))
                              (:div :class "bottom" :id "left-bottom"))
                        (:div :id "right" :class "column"
                              (:div :class "top-right" "top right")
                              (:div :class "bottom" :id "right-bottom"))))))))))

(define-easy-handler (content-viewer-page :uri "/main-js") ()
  "HTTP endpoint for content-viewer page"
  (make-content-viewer-page-use-js))

(defun get-version ()
  "0.14")

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
