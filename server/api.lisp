(in-package #:content-viewer)

(define-api-endpoint file-data *file-api-endpoint* ()
  "REST endpoint for files"
  (case verb
    (:delete
     (file-data-delete raw-data))
    ))


