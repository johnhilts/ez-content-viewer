(in-package #:content-viewer)

;;;;
; - I/O for favorites
; - API for favorites
; - search by name
; - retrieve entire list
;;;;

;; (defun get-favorite-by-id (id)
;;   "get favorite by unique ID"
;;   (let ((favorites (test-json-data)))
;;          (find-if
;;           #'(lambda (e)
;;               (let ((search-id (getf e :id)))
;;                 (equal search-id id))) favorites)))

(defun get-favorite (id)
  "get favorite by ID then convert to json"
  (let ((favorite (get-favorite-by-id id)))
    (json:encode-json-plist-to-string favorite)))

(defun fetch-or-create-favorites ()
  "get favorite from persisted data"
  (fetch-or-create-data *favorite-file-path*))

(defun get-favorite-list ()
  "get favorite list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-favorites)))

(defun favorite-data-get-handler (id)
  "get favorite by ID or entire list - API level handler"
  (if id
      (get-favorite (parse-integer id))
      (get-favorite-list)))

(defun favorite-data-add (model)
    "add favorite data to persisted data"
  (let ((new-id (getf model :id))
        (existing-favorites (fetch-or-create-favorites)))
    (write-complete-file *favorite-file-path* (append existing-favorites (list model)))
    (json:encode-json-to-string (list new-id))))

(defun favorite-data-update (model)
  "update favorite data and persisted data"
  (let* ((update-id (getf model :id))
         (existing-favorites (fetch-or-create-favorites))
         (updated-item-position (position-if #'(lambda (e) (= (getf e :id) update-id)) existing-favorites))
         (updated-favorites (splice-and-replace-item-in-list existing-favorites model updated-item-position)))
    
    (write-complete-file *favorite-file-path* updated-favorites)
    (json:encode-json-to-string (list update-id))))

(define-data-update-handler favorite-data-add-handler (model)
    "add favorite data to persisted data - API level handler"
    (favorite-data-add model))

(define-data-update-handler favorite-data-update-handler (model)
  "update favorite data and persisted data - API level handler"
  (favorite-data-update model))

(define-data-update-handler favorite-data-delete-handler (model)
  "delete favorite by ID - API level handler"
  (let ((delete-id (getf model :id)))
    (when delete-id
      (let* ((existing-favorites (fetch-or-create-favorites))
             (deleted-item-position (position-if #'(lambda (e) (= (getf e :id) delete-id)) existing-favorites))
             (updated-favorites (splice-and-remove-item-in-list existing-favorites deleted-item-position)))
        (write-complete-file *favorite-file-path* updated-favorites)
        (json:encode-json-to-string (list delete-id))))))

(defun get-favorite-by-name (favorite-name)
  (let* ((existing-favorites (fetch-or-create-favorites))
        (position (position-if #'(lambda (e) (string-equal (getf e :name) favorite-name)) existing-favorites)))
    (when position
      (nth position existing-favorites))))

(defun get-next-index-for-favorites (favorite-list)
  "calculate next index for favorite list"
  (let ((id-list (mapcar #'(lambda (e) (getf e :id)) favorite-list)))
    (if (length id-list)
        (+ 1 (apply #'max id-list))
        1)))

(defun add-favorite-from-uploaded-file (favorite-name uploaded-file-name)
  (let ((uploaded-file-name-web-path (subseq (namestring uploaded-file-name) (search (subseq *share-root* 1) (namestring uploaded-file-name))))
        (existing-favorite (get-favorite-by-name favorite-name)))
    (if existing-favorite
        (progn
          (push uploaded-file-name-web-path (getf existing-favorite :files))
          (favorite-data-update existing-favorite))
        (let ((new-favorite (list :id (get-next-index-for-favorites (fetch-or-create-favorites)) :name favorite-name :files (list uploaded-file-name-web-path))))
          (favorite-data-add new-favorite)))))

(define-info-class favorite path content-type)

(defun get-favorite-name-list ()
  (mapcar #'(lambda (e)
              (let ((path (getf e :name))
                    (content-type 'favorite)
                    (favorite (make-instance 'favorite-info))) 
                (populate-info-object favorite path content-type)))
          (fetch-or-create-favorites)))

(defmethod create-javascript-object ((item favorite-info) ignored-param)
  "create javascript object to be added to a list of favorite categories"
  (let* ((favorite-path (favorite-path item))
         (favorite-content-type (favorite-content-type item)))
    `(create
      :path ,favorite-path
      ,(symbol-to-js-string :content-type) ,(symbol-to-js-string favorite-content-type))))
