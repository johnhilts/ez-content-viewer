(in-package #:content-viewer)

(define-info-class file path timestamp content-type)
(define-info-class content folders images videos) ;; note: I can't inline this - it will break compilation if I don't do this on the top level

(defun get-content-files (directory)
  (flet ((get-content-files-by-type (wildcards content-type)
           (mapcar #'(lambda (path)
                       (let ((timestamp (get-universal-time))
                             (file (make-instance 'file-info)))
                         (populate-info-object file path timestamp content-type)))
                   (remove-if #'null
                              (mapcan #'(lambda (wildcard) (directory (format nil "~a/~a" directory wildcard))) wildcards)))))
    (let ((folder-mask '("*"))
          (image-mask '("*.png" "*.jpg"))
          (video-mask '("*.mov" "*.mp4")))
      (multiple-value-bind
            (folders images videos)
          (values
           (get-content-files-by-type folder-mask 'folder)
           (get-content-files-by-type image-mask 'image)
           (get-content-files-by-type video-mask 'video))
        (let ((content (make-instance 'content-info)))
          (populate-info-object content folders images videos))))))

(defmethod index-folders ((content-info content-info))
  (let* ((content-folders (content-folders content-info))
         (folder-count (length content-folders)))
    (make-array folder-count :element-type 'pathname :initial-contents content-folders)))

(defparameter *content-root* "./media"
  "root of content media")

(defparameter *folders* (index-folders (get-content-files "./media"))
  "array of indexed folders")
