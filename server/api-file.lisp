(in-package #:content-viewer)

(define-data-update-handler file-data-delete (model)
  "delete file by path"
  (let ((delete-web-path (getf model :file-path)))
    (when delete-web-path
      (let ((delete-result (delete-web-path delete-web-path)))
        (json:encode-json-to-string (list (namestring delete-result)))))))
