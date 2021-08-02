(in-package #:content-viewer)

(defun client-ui ()
  "define client side UI functions"
  (ps

    ;; (defparameter *todo-checkbox* "todo-check")
    ;; (defparameter *todo-label* "todo-label")
    ;; (defparameter *show-todo-edit* "show-todo-edit")
    ;; (defparameter *hide-todo-edit* "hide-todo-edit")
    ;; (defparameter *todo-text* "todo-text")

    (setf (chain window onload) init)))

(define-for-ps clear-field (field)
  "clear input field's value"
  (setf (chain field value) "")
  t)
    
(define-for-ps clear-children (parent-element)
  "remove all child nodes of a parent element"
  (while (chain parent-element (has-child-nodes))
    (chain parent-element (remove-child (@ parent-element first-child)))))
    
(define-for-ps init ()
  "initialize html elements and JS objects on page load"
  ;; (get-app-settings-from-server)
  ;; (get-todo-list-from-server)
  ;; (setf add-button (chain document
  ;;                         (get-element-by-id "todo-add-btn")))
  ;; (chain add-button
  ;;        (add-event-listener "click" add-todo false))
  (render-file-list folder-list)
  (render-file-list image-list)
  (render-file-list video-list)
  )

;; (define-for-ps render-app-settings ()
;;   "render html elements for app settings"
;;   (let ((parent-element (chain document (get-element-by-id "app-settings"))))
;;     (jfh-web::with-html-elements
;;         (div
;;          (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)") (checked . "(@ *app-settings* hide-done-items)"))
;;          (label (for . "hide-done") "Hide Done Items.")))))

;; (define-for-ps filter-todos ()
;;   (let* ((filter-text (@ (chain document (get-element-by-id "todo-filter-text")) value))
;;          (filtered-todos (chain todo-list (filter (lambda (todo) (chain (@ todo text) (match (new (-reg-exp filter-text "i")))))))))
;;     (render-todo-list filtered-todos)
;;     (update-app-settings))
;;   t)

;; (define-for-ps render-todo-filter ()
;;   "render html elements for todo filter"
;;   (let ((parent-element (chain document (get-element-by-id "todo-filter"))))
;;     (jfh-web::with-html-elements
;;         (div
;;          (div
;;           (span (br (ref . "br")))
;;           (input (id . "todo-filter-text") (type . "textbox") (placeholder . "Enter text to filter on here") (value . "(@ *app-settings* filter-text)")))
;;          (span "  ")
;;          (button (onclick . "(filter-todos)") "Filter"))))
;;   t)

(Define-For-ps render-full-size (item-url)
  (let ((parent-element (chain document (get-element-by-id "full-size-parent")))
        (file-list-div (chain document (get-element-by-id "file-list"))))
    (flet ((toggle-full-size-visibility (show-full-size)
             (setf (@ parent-element hidden) (not show-full-size))
             ;; (setf (@ file-list-div hidden) show-full-size)
             (setf (@ file-list-div style) (if show-full-size "display:none;" "display:flex;"))))
      (let ((file-img-style ""))
        (toggle-full-size-visibility t)
        (clear-children parent-element)
        (jfh-web::with-html-elements
            (div (id . "full-size-main")
                 (onclick . "(toggle-full-size-visibility nil)")
                 (img (src . "(progn item-url)") (width . "100%") (height . "100%") (style . "(progn file-img-style)"))
                 (span (br " "))
                 (span "(progn item-url)")))
        t))))

(define-for-ps render-file-list (image-list)
  "render html elements for file list"
  (flet ((show-in-preview-pane (file)
           (alert "hi")
           t)
         (format-file-text (file)
           (let* ((file-path (@ file path))
                 (file-content-type (@ file content-type))
                 (file-text (cond
                              ((equal 'image file-content-type)
                               "(i)")
                              ((equal 'video file-content-type)
                               "(v)")
                              (t ""))))
             (if (equal 'folder file-content-type)
                 (+ "[ " file-path " ]")
                 (+ file-path " " file-text)))))
    (let* ((file-list-div (chain document (get-element-by-id "left-bottom")))
           (parent-element file-list-div))
      ;; (clear-children parent-element)
      (chain image-list
             (map
              #'(lambda (file index)
                  (let ((file-text (format-file-text file)))
                    (jfh-web::with-html-elements
                        (div (class . "column-item")
                             (a (onclick . "(render-preview-pane file)") "(progn file-text)")))
                    t)))))))

(define-for-ps render-preview-pane (file)
  "render html elements for file pane"
  (let ((parent-element (chain document (get-element-by-id "right-bottom"))))
    (clear-children parent-element)
    (let* ((file-text (@ file path))
           (file-img-style "") ;; (+ "transform: rotate(" (- 360 270) "deg)")))
           (request-folder-index (chain (@ location search) (match (new (-reg-exp "fi=(\\d)")))))
           (current-index (if request-folder-index (parse-int (@ request-folder-index 1)) 0)))
      (cond
        ((equal 'folder (@ file content-type))
         (setf (@ location href) (+ "main-js?fi=" (@ file folder-index) "&ci=" current-index)))
        ((equal 'image (@ file content-type)) 
         (jfh-web::with-html-elements
             (div (class . "column-item")
                  (a
                   (onclick . "(render-full-size (@ file path))")
                   (img (src . "(@ file path)") (style . "(progn file-img-style)") (width . "200") (height . "200"))
                   (span (br " "))
                   "(progn file-text)"))))
        ((equal 'video (@ file content-type))
         (jfh-web::with-html-elements
             (div (class . "column-item")
                  (a
                   (onclick . "(render-full-size (@ file path))")
                   (video (src . "(@ file path)") (width . "200") (height . "200") (type . "video/mov") (controls . "true") (autoplay . "true"))
                   (span (br " "))
                   "(progn file-text)")))))
      t)))


