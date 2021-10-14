(in-package #:content-viewer)

(define-api-endpoint favorite-data *favorite-api-endpoint* (id)
  "REST endpoint for favorites"
  (case verb
    (:put
     (favorite-data-update-handler raw-data))
    (:post
     (favorite-data-add-handler raw-data))
    (:delete
     (favorite-data-delete-handler raw-data))
    (:get
     (favorite-data-get-handler id))))

(define-api-endpoint file-data *file-api-endpoint* ()
  "REST endpoint for files"
  (case verb
    (:delete
     (file-data-delete-handler raw-data))
    ))


