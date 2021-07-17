(in-package #:content-viewer)

(define-info-class file path timestamp content-type)
(define-info-class content folders images videos) ;; note: I can't inline this - it will break compilation if I don't do this on the top level

(defmethod get-content-timestamp ((file-info file-info))
  (case (file-content-type file-info)
     (let ((exif (make-exif (file-path file-info))))
       (exif-value :DateTimeOriginal exif)))
    (image
    (video
     (format t "this is a video"))
    (otherwise
     (format t "this is something else"))))

(defun get-content-files (directory)
  (defun get-content-files-by-type (wildcards content-type)
    (flet ((get-pathnames-by-type (wildcards)
             (remove-if #'null
                        (mapcan #'(lambda (wildcard) (directory (format nil "~a/~a" directory wildcard))) wildcards)))
           (get-file-info (path)
             (let ((timestamp (get-universal-time))
                   (file (make-instance 'file-info)))
               (populate-info-object file path timestamp content-type))))
      (mapcar #'get-file-info (get-pathnames-by-type wildcards))))
  (let ((folders (get-content-files-by-type  '("*") 'folder))
        (images (get-content-files-by-type '("*.png" "*.jpg" "*.PNG" "*.JPG") 'image))
        (videos (get-content-files-by-type '("*.mov" "*.mp4") 'video))
        (content (make-instance 'content-info)))
    (populate-info-object content folders images videos)))

(defmethod index-folders ((content-info content-info))
  (let* ((content-folders (content-folders content-info))
         (folder-count (length content-folders)))
    (make-array folder-count :element-type 'pathname :initial-contents content-folders)))

(defparameter *content-root* "./media"
  "root of content media")

(defparameter *folders* (index-folders (get-content-files "./media"))
  "array of indexed folders")
