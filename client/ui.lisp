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
  (get-favorite-list-from-server)
  ;; (setf add-button (chain document
  ;;                         (get-element-by-id "todo-add-btn")))
  ;; (chain add-button
  ;;        (add-event-listener "click" add-todo false))
  (render-favorites-link)
  (render-share-link)
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

(define-for-ps delete-file-ui (file-url &optional do-after-delete)
  (if (confirm "Are you sure you want to delete this?")
      (progn
        (delete-file-by-url file-url)
        (if do-after-delete
            (funcall do-after-delete)) 
        t)
      nil))

(define-For-ps render-favorites-link ()
  (flet ((prepare-for-favorite-list ()
           "clears out the file list - this might not even be necessary..."
           (let* ((file-list-div (chain document (get-element-by-id "left-bottom")))
                  (preview-div (chain document (get-element-by-id "right-bottom"))))
             (clear-children file-list-div)
             (clear-children preview-div)
             (render-file-list favorite-name-list))))
    (let ((parent-element (chain document (get-element-by-id "top-right"))))
      (jfh-web::with-html-elements
          (span (a (onclick . "(prepare-for-favorite-list)") "Favorites")))))
  t)

(define-For-ps render-share-link ()
  (let ((parent-element (chain document (get-element-by-id "top-right"))))
    (jfh-web::with-html-elements
        (span (style . "margin-left:25px;") (a (style . "text-decoration: none;") (href . "/share") "Share"))))
  t)

(define-For-ps render-full-size (item-url created-date &optional do-after-delete)
  (let ((parent-element (chain document (get-element-by-id "full-size-parent")))
        (file-list-div (chain document (get-element-by-id "file-list"))))
    (labels ((toggle-full-size-visibility (show-full-size)
               (setf (@ parent-element hidden) (not show-full-size))
               (setf (@ file-list-div style) (if show-full-size "display:none;" "display:flex;")))
             (delete-file-from-full-size-view (item-url)
               (if (delete-file-ui item-url)
                   (progn
                     (delete-file-by-url item-url)
                     (toggle-full-size-visibility nil)
                     (if do-after-delete (funcall do-after-delete)))
                   t)))
      
      (let ((file-img-style ""))
        (toggle-full-size-visibility t)
        (clear-children parent-element)
        (jfh-web::with-html-elements
            (div (id . "full-size-main")
                 (onclick . "(toggle-full-size-visibility nil)")
                 (img (src . "(progn item-url)") (width . "100%") (height . "100%") (style . "(progn file-img-style)"))
                 (span (br " "))
                 (span "(progn item-url)")
                 (span (br " "))
                 (span "(progn created-date)")
                 (span (br " ") (br " "))
                 (button (onclick . "(delete-file-from-full-size-view item-url)") "Delete")))
        t))))

(define-for-ps get-favorite-file-list (selected-favorite)
  (labels
      ((favorite-to-file (file) (create "path" file "contentType" 'image))
       (favorite-files (favorite) (@ favorite files))
       (favorite-by-name (favorite) (equal (@ selected-favorite path) (@ favorite name))))
    (let* ((favorite-by-name (chain favorite-list (filter #'favorite-by-name)))
           (favorite-files (aref (chain favorite-by-name (map #'favorite-files)) 0))
           (favorite-as-files (chain favorite-files (map #'favorite-to-file))))
      favorite-as-files)))

(define-for-ps render-file-list (file-list)
  "render html elements for file list"
  (flet ((format-file-text (file)
           (let* ((file-path (@ file path))
                  (file-content-type (@ file content-type))
                  (file-text (cond
                               ((equal 'image file-content-type)
                                "📷")
                               ((equal 'video file-content-type)
                                "🎥")
                               (t ""))))
             (cond
               ((equal 'folder file-content-type) (+ "[ " file-path " ] 🗃 "))
               ((equal 'favorite file-content-type) (+ "[ " file-path " ] ❤"))
               (t (+ file-path " " file-text))))))
    (let* ((file-list-div (chain document (get-element-by-id "left-bottom")))
           (parent-element file-list-div))
      (chain file-list
             (map
              #'(lambda (file index)
                  (let* ((file-text (format-file-text file))
                        (item-id (+ "item-" (chain index (to-string))))
                        (onclick-handler #'(lambda ()
                                             (if (equal 'favorite (@ file content-type))
                                                 (progn
                                                   (clear-children parent-element)
                                                   (render-file-list (get-favorite-file-list file)))
                                                 (render-preview-pane file item-id)))))
                    (jfh-web::with-html-elements
                        (div (class . "column-item")
                             (a
                              (id . "(progn item-id)")
                              (onclick . "(onclick-handler)")
                              "(progn file-text)")))
                    t)))))))

(define-for-ps render-preview-pane (file item-id)
  "render html elements for file pane"
  (labels ((get-file-dimensions (height width is-landscape)
             (let* ((basic-dimension (if is-landscape 600 400))
                    (dimension-info
                     (if (> height width)
                         (create :dimension (* basic-dimension (/ width height)) :max 'height)
                         (create :dimension (* basic-dimension (/ height width)) :max 'width))))
               (create
                :height (if (equal (@ dimension-info :max) 'height) basic-dimension (@ dimension-info :dimension))
                :width (if (equal (@ dimension-info :max) 'width)  basic-dimension (@ dimension-info :dimension)))))
           (clear-all-selected ()
             "typically there should only be 1 selected at a time, but this will select and clear all that have been styled as selected"
             (chain (chain document (query-Selector-All "a.column-item-selected")) (for-each #'(lambda (e) (setf (@ e class-name) "")))))
           (close-add-to-favorite ()
             (setf (@ (@ (chain document (get-element-by-id "overlay")) style) display) "none"))
           (add-favorite-entry-handler (favorite-id file-path event)
             (chain event (prevent-default))
             (add-favorite-entry favorite-id file-path)
             (close-add-to-favorite))
           (add-favorite-search-result (favorite-name favorite-id file-path)
             (let ((parent-element (chain document (get-element-by-id "search-favorite-results"))))
               (jfh-web::with-html-elements
                   (span (a (onclick . "(add-favorite-entry-handler favorite-id file-path)") favorite-name) (br " "))))))
    (flet ((highlight-selected-element ()
             (let ((selected-element (chain document (get-element-by-id item-id))))
               (clear-all-selected)
               (setf (@ selected-element class-name) "column-item-selected")))
           (add-to-favorite ()
             (setf (@ (@ (chain document (get-element-by-id "overlay")) style) display) "block")
             (chain (chain document (get-element-by-id "favorite-search")) (focus))
             t)
           (search-favorites (file-path event)
             (clear-children (chain document (get-element-by-id "search-favorite-results")))
             (let ((search-input (chain (+ (@ (chain document (get-element-by-id "favorite-search")) value) (@ event key)) (to-lower-case))))
               (chain (chain favorite-list
                             (filter #'(lambda (favorite) (>= (chain (chain (@ favorite name) (to-lower-case)) (index-of search-input)) 0))))
                      (map #'(lambda (favorite) (add-favorite-search-result (@ favorite name) (@ favorite id) file-path) t))))
             t)
           (add-favorite-item-handler (file-path evt)
             (chain evt (prevent-default))
             (add-favorite-item file-path)
             (close-add-to-favorite)))
      (let* ((parent-element (chain document (get-element-by-id "right-bottom")))
             (clear-children-closure #'(lambda () (clear-children parent-element))))
        (funcall clear-children-closure)
        (highlight-selected-element)
        (let* ((file-text (@ file path))
               (file-created (@ file created))
               (file-img-style "") ;; (+ "transform: rotate(" (- 360 270) "deg)")))
               (request-folder-index (chain (@ location search) (match (new (-reg-exp "fi=(\\d)")))))
               (current-index (if request-folder-index (parse-int (@ request-folder-index 1)) 0)))
          (cond
            ((equal 'folder (@ file content-type))
             (setf (@ location href) (+ "main?fi=" (@ file folder-index) "&ci=" current-index)))
            ((equal 'image (@ file content-type))
             (let* ((element-id (+ "file-preview" item-id (random 10000000)))
                    (delete-file-closure #'(lambda ()
                                             (let ((item-element (chain document (get-element-by-id item-id))))
                                               (setf (@ item-element style) "text-decoration: line-through;color:gray;")
                                               (funcall clear-children-closure))))
                    (is-landscape (not (= *orientation-rotation-minus-90* (@ file orientation))))
                    (file-dimensions
                     (if (and (@ file image-length) (@ file image-width))
                         (get-file-dimensions (@ file image-length) (@ file image-width) is-landscape)))
                    (image-height
                     (if (@ file image-length)
                         (if (= *orientation-rotation-minus-90* (@ file orientation))
                             (@ file-dimensions height)
                             (@ file-dimensions width))))
                    (image-width
                     (if (@ file image-width)
                         (if (= *orientation-rotation-minus-90* (@ file orientation))
                             (@ file-dimensions width)
                             (@ file-dimensions height)))))
               (jfh-web::with-html-elements
                   (div (class . "column-item")
                        (a
                         (href . "(@ file path)")
                         ;; (onclick . "(render-full-size (@ file path) file-created delete-file-closure)")
                         (img (src . "(@ file path)") (width . "(or image-height 600)") (height . "(or image-width 400)") (style . "(progn file-img-style)"))
                         (span (br " "))
                         "(progn file-text)"
                         (span (br " "))
                         "(progn file-created)")
                        (span (br " ") (br " "))
                        (button (onclick . "(delete-file-ui (@ file path) delete-file-closure)") "Delete")
                        (span " ")
                        (button (onclick . "(add-to-favorite)") "+")
                        (div (id . "overlay")
                             (div (id . "text")
                                  (p (a (onclick . "(close-add-to-favorite)") "x"))
                                  (h2 "Add to Favorites")
                                  (input (id . "favorite-search") (placeholder . "search") (onkeypress . "(search-favorites (@ file path))"))
                                  (span " " (button (onclick . "(add-favorite-item-handler (@ file path))") "add new") (br " "))
                                  (div (id . "search-favorite-results"))))))))
            ((equal 'video (@ file content-type))
             (jfh-web::with-html-elements
                 (div (class . "column-item")
                      (a
                       (video (id . "(progn element-id)") (src . "(@ file path)") (width . "600") (height . "400") (type . "video/mov") (controls . "true") (autoplay . "true"))
                       (span (br " "))
                       "(progn file-text)"
                       (span (br " "))
                       "(progn file-created)"
                       (span (br " ") (br " "))
                       (button (onclick . "(delete-file-ui (@ file path))") "Delete"))))))
          t)))))


