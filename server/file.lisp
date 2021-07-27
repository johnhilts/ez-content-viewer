(in-package #:content-viewer)

(define-info-class file path timestamp content-type)
(define-info-class content folders images videos) ;; note: I can't inline this - it will break compilation if I don't do this on the top level

(defmethod get-content-timestamp ((file-info file-info)) ; maybe convert this to defun and take 2 parameters then return timestamp and geo lat+lng
  "get filestamp for a file"
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
               (format nil "this is a jpg without exif")))
           (format nil "this is an image without exif"))))
    (video
     (format nil "this is a video"))
    (otherwise
     (format nil "this is something else"))))

;; this is an example - it needs to be included in an orchestrator / imperative shell type function
(defun get-file-list (&optional (directory *content-root*))
  (handler-bind ((ZPB-EXIF:INVALID-EXIF-STREAM
                  (lambda (condition)
                    (declare (ignore condition))
                    (invoke-restart 're-start-exif-jpg)))) ;; invoke "emergency" function
    (let ((content-files (get-content-files directory)))
      (mapc #'(lambda (e)
                (setf (file-timestamp e) (get-content-timestamp e)))
            (content-images content-files))
      content-files)))

(defun get-content-files (directory)
  "get the content files in a directory"
  (let ((directory (if (stringp directory) directory (file-path directory))))
    (defun get-content-files-by-type (wildcards content-type)
      "get files by extension (type)"
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
          (videos (get-content-files-by-type '("*.mov" "*.mp4" "*.MOV" "*.MP4") 'video))
          (content (make-instance 'content-info)))
      (populate-info-object content folders images videos))))

(defun search-folders (search-path file-info &optional (index 0))
  "version of search that works with file-info list"
  (cond
    ((null file-info)
     nil)
    ((equal search-path (file-path (car file-info)))
     index)
    (t (search-folders search-path (cdr file-info) (+ 1 index)))))

(defmethod pathname= ((file-info1 file-info) (file-info2 file-info))
  (string-equal
   (namestring (file-path file-info1))
   (namestring (file-path file-info2))))

(defmethod index-folders ((content-info content-info) &optional (folders nil))
  "index (cache) the folders"
  (flet ((append-new-items (new old)
           "Similar to union, but I want to preserve the order of original list's items"
           (append old (set-difference (union old new :test #'pathname=) old :test #'pathname=))))
    (let ((content-folders (content-folders content-info)))
      (adjoin *content-root* (append-new-items content-folders (cdr folders))))))

(defparameter *content-root* "./media"
  "root of content media")

(defparameter *folders* (index-folders (get-content-files "./media"))
  "array of indexed folders")
