(in-package #:content-viewer)

(define-api-endpoint favorite-data *favorite-api-endpoint* (id)
  "REST endpoint for favorites"
  (case verb
    (:put
     (favorite-data-update raw-data))
    (:post
     (favorite-data-add raw-data))
    (:delete
     (favorite-data-delete raw-data))
    (:get
     (favorite-data-get id))))

(define-api-endpoint file-data *file-api-endpoint* ()
  "REST endpoint for files"
  (case verb
    (:delete
     (file-data-delete raw-data))
    ))


