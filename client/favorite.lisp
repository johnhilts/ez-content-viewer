(in-package #:content-viewer)

(defun client-favorite ()
  "define client side functions to handle favorites"
  (ps
    (defvar favorite-list ([]))))

(define-for-ps send-new-favorite-item-to-server (favorite-item call-back)
  "save new favorite on server"
  (send-to-server *favorite-api-endpoint* "POST" favorite-item call-back))

(define-for-ps send-updated-favorite-item-to-server (favorite-item)
  "save updated favorite on server"
  (send-to-server *favorite-api-endpoint* "PUT" favorite-item))

(define-for-ps delete-favorite-item-on-server (delete-id-object call-back)
  "save updated favorite on server"
  (send-to-server *favorite-api-endpoint* "DELETE" delete-id-object call-back))

(define-for-ps get-favorite-list-from-server (&optional optional-call-back)
  "define callback and get favorite list from server and re-render html elements"
  (with-callback
      (get-from-server *favorite-api-endpoint*)
    (let ((server-favorite-list (chain -j-s-o-n (parse (@ this response-text)))))
      ;; (render-favorite-list server-favorite-list)
      (setf favorite-list server-favorite-list)
      (when optional-call-back
        (optional-call-back))))
  t)
    
(define-for-ps get-new-favorite (existing-favorites favorite-name)
  "get new favorite entry without any paths"
  (let* ((next-id (get-next-index existing-favorites)))
    (create id next-id name favorite-name files '())))

(define-for-ps find-favorite-by-id (existing-favorites favorite-id)
  "find favorite by ID"
  (chain existing-favorites (find #'(lambda (e) (equal favorite-id (@ e id))))))
  
(define-for-ps add-new-favorite-entry (existing-favorites favorite-id file-path)
  "add new favorite entry for existing favorite"
  (flet ((set-null-to-empty-array ()
           (setf (@ favorite-item files) (or (@ favorite-item files) ([])))))
    (let ((favorite-item (find-favorite-by-id existing-favorites favorite-id)))
      (set-null-to-empty-array)
      (chain (@ favorite-item files) (push file-path))))
  t)

(define-for-ps add-favorite-item (file-path evt)
  "add favorite item on client and server"
  (chain evt (prevent-default))
  (let* ((favorite-input (chain document (get-element-by-id "favorite-search")))
         (favorite-text (chain favorite-input value)))
    (with-callback
        (get-favorite-list-from-server)
      (let ((favorite-item (get-new-favorite favorite-list favorite-text)))
        (chain favorite-list (push favorite-item))
        ;; HIDE FAVORITE INPUT (clear-field favorite)
        ;; (render-favorite-list favorite-list)
        (with-callback
            (send-new-favorite-item-to-server favorite-item)
          (add-favorite-entry (@ favorite-item id) file-path)))))
  t)

(define-for-ps add-favorite-entry (favorite-id file-path)
  "add favorite entry on client and server"
  ;; (chain evt (prevent-default))
  (with-callback
      (get-favorite-list-from-server)
    (let ((favorite-item (find-favorite-by-id favorite-list favorite-id)))
      (add-new-favorite-entry favorite-list favorite-id file-path)
      ;; (clear-field favorite)
      ;; (render-favorite-list favorite-list)
      (send-updated-favorite-item-to-server favorite-item)))
  t)

(define-for-ps update-favorite (index favorite-id)
      "update favorite on client and server"
      (let* ((favorite-list-index (@ (chain favorite-list (find-index #'(lambda (favorite) (= favorite-id (@ favorite id)))))))
             (favorite-item (aref favorite-list favorite-list-index)))
        (send-updated-favorite-item-to-server favorite-item))
      t)

(define-for-ps update-favorite-from-edit (favorite)
  "update favorite on client and server"
  (send-updated-favorite-item-to-server favorite)
  ;; (render-favorite-list favorite-list)
  t)

(define-for-ps delete-favorite-by-id (delete-id)
  "delete favorite on client and server"
  (delete-favorite-item-on-server (create id delete-id))
  (let ((delete-item-index (chain favorite-list (find-index #'(lambda (favorite) (= (@ favorite id) delete-id))))))
    (chain favorite-list (splice delete-item-index 1)))
  ;; (render-favorite-list favorite-list)
  t)
    
