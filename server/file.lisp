(in-package #:content-viewer)

(define-info-class file path timestamp content-type)
(define-info-class content folders images videos) ;; note: I can't inline this - it will break compilation if I don't do this on the top level

(defmethod get-content-timestamp ((file-info file-info)) ; maybe convert this to defun and take 2 parameters then return timestamp and geo lat+lng
  (case (file-content-type file-info)
    (image
     (let* ((file-path (file-path file-info))
            (extension (pathname-type file-path)))
       (if (or (string-equal "jpg" extension) (string-equal "jpeg" extension))
           (restart-case
               (let ((exif (make-exif (file-path file-info))))
                 (exif-value :DateTimeOriginal exif))
             (re-start-exif-jpg ()
               :report "jpg ZPB-EXIF:INVALID-EXIF-STREAM" ; how do I get this dynamically?
               (format t "this is a jpg without exif")))
           (format t "this is an image without exif"))))
    (video
     (format t "this is a video"))
    (otherwise
     (format t "this is something else"))))

;; this is an example - it needs to be included in an orchestrator / imperative shell type function
(handler-bind ((ZPB-EXIF:INVALID-EXIF-STREAM
                (lambda (condition)
                  (declare (ignore condition))
                  (invoke-restart 're-start-exif-jpg)))) ;; invoke "emergency" function
  (let ((jpg-without-exif-example (car (content-images (get-content-files "media/photos")))))
    (get-content-timestamp jpg-without-exif-example)))

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
