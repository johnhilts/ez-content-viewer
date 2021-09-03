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

(defun favorite-data-get (id)
  "get favorite by ID or entire list"
  (if id
      (get-favorite (parse-integer id))
      (get-favorite-list)))

(define-data-update-handler favorite-data-add (model)
    "add favorite data to persisted data"
  (let ((new-id (getf model :id))
        (existing-favorites (fetch-or-create-favorites)))
    (write-complete-file *favorite-file-path* (append existing-favorites (list model)))
    (json:encode-json-to-string (list new-id))))

(define-data-update-handler favorite-data-update (model)
  "update favorite data and persisted data"
  (let* ((update-id (getf model :id))
         (existing-favorites (fetch-or-create-favorites))
         (updated-item-position (position-if #'(lambda (e) (= (getf e :id) update-id)) existing-favorites))
         (updated-favorites (splice-and-replace-item-in-list existing-favorites model updated-item-position)))
    
    (write-complete-file *favorite-file-path* updated-favorites)
    (json:encode-json-to-string (list update-id))))

(define-data-update-handler favorite-data-delete (model)
  "delete favorite by ID"
  (let ((delete-id (getf model :id)))
    (when delete-id
      (let* ((existing-favorites (fetch-or-create-favorites))
             (deleted-item-position (position-if #'(lambda (e) (= (getf e :id) delete-id)) existing-favorites))
             (updated-favorites (splice-and-remove-item-in-list existing-favorites deleted-item-position)))
        (write-complete-file *favorite-file-path* updated-favorites)
        (json:encode-json-to-string (list delete-id))))))
