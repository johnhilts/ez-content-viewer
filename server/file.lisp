(in-package #:content-viewer)

(define-info-class file folders images videos) ;; note: I can't inline this - it will break compilation if I don't do this on the top level

(defun get-files (directory)
  (flet ((get-files-by-type (wildcards)
           (remove-if #'null (mapcan #'(lambda (wildcard) (directory (format nil "~a/~a" directory wildcard))) wildcards))))
    (let ((folder-mask '("*"))
          (image-mask '("*.png" "*.jpg"))
          (video-mask '("*.mov" "*.mp4")))
      (multiple-value-bind
            (folders images videos)
            (values
             (cons directory (get-files-by-type folder-mask))
             (get-files-by-type image-mask)
             (get-files-by-type video-mask))
            (let ((file (make-instance 'file-info)))
              (populate-info-object file folders images videos))))))

(defmethod index-folders ((file-info file-info))
  (let* ((file-folders (file-folders file-info))
         (folder-count (length file-folders)))
    (make-array folder-count :element-type 'pathname :initial-contents file-folders)))
