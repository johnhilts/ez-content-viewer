(in-package #:content-viewer)

(define-info-class file path timestamp content-type alias-path image-length image-width image-orientation signaled-error)
(define-info-class content folders images videos) ;; note: I can't inline this - it will break compilation if I don't do this on the top level

(defparameter *content-root* "./media"
  "root of content media")

(defmethod pathname= ((file-info1 file-info) (file-info2 file-info))
  (string-equal
   (namestring (file-path file-info1))
   (namestring (file-path file-info2))))

(defun get-content-files (directory)
  "get the content files in a directory"
  (let ((directory (if (stringp directory) directory (file-path directory))))
    (defun get-content-files-by-type (wildcards content-type)
      "get files by extension (type)"
      (flet ((get-pathnames-by-type (wildcards)
               (remove-if #'null
                          (mapcan #'(lambda (wildcard) (directory (format nil "~a/~a" directory wildcard) :resolve-symlinks nil)) wildcards)))
             (get-file-info (file-path)
               (let* ((timestamp (get-universal-time))
                      (path (truename file-path))
                      (alias-path (if (not (equal (namestring path) (namestring file-path)))
                                      file-path
                                      nil))
                      (image-length 400)
                      (image-width 600)
                      (image-orientation *orientation-rotation-minus-90*)
                      (signaled-error nil)
                      (file (make-instance 'file-info)))
                 (populate-info-object file path timestamp content-type alias-path image-length image-width image-orientation signaled-error))))
        (mapcar #'get-file-info (get-pathnames-by-type wildcards))))
    (let ((folders (get-content-files-by-type  '("*") 'folder))
          (images (get-content-files-by-type '("*.png" "*.jpg" "*.PNG" "*.JPG") 'image))
          (videos (get-content-files-by-type '("*.mov" "*.mp4" "*.MOV" "*.MP4") 'video))
          (content (make-instance 'content-info)))
      (populate-info-object content folders images videos))))

(defun index-alias-folders (folders)
  "index (cache) the alias for any folders that have them"
  (remove-if #'(lambda (e) (null (cdr e)))
             (mapcar #'(lambda (e) (cons (file-path e) (file-alias-path e)))
                     (cdr folders))))

(defun get-physical-path (web-path)
  (let* ((search-path (subseq *content-root* (position #\/ *content-root*)))
         (physical-relative-path-start (search search-path web-path)))
    (when physical-relative-path-start
      (probe-file (format nil ".~a" web-path)))))

(defun delete-web-path (web-path)
  (delete-file
   (get-physical-path web-path)))

(defun get-aias-path (file-path aliased-folders)
  (let ((aliased-path (find-if #'(lambda (e) (search (namestring (car e)) (namestring file-path))) aliased-folders)))
    (when aliased-path
      (string-replace (namestring file-path) (namestring (car aliased-path)) (namestring (cdr aliased-path))))))

(defmethod index-folders ((content-info content-info) &optional (folders nil))
  "index (cache) the folders"
  (flet ((append-new-items (old new)
           "Similar to union, but I want to preserve the order of original list's items"
           (append old (set-difference (union old new :test #'pathname=) old :test #'pathname=))))
    (let ((content-folders (content-folders content-info)))
      (adjoin *content-root* (append-new-items (cdr folders) content-folders)))))

(defparameter *folders* (index-folders (get-content-files "./media"))
  "array of indexed folders")

(defmethod get-content-image-dimensions ((file-info file-info) exif-by-file) ; maybe convert this to defun and take 2 parameters then return timestamp and geo lat+lng
  "get image dimensions for a jpg"
  (let* ((file-path (file-path file-info))
         (extension (pathname-type file-path)))
    (if (or (string-equal "jpg" extension) (string-equal "jpeg" extension))
        (restart-case
            (let ((exif (funcall exif-by-file (file-path file-info))))
              (values (getf exif :image-length) (getf exif :image-width) (getf exif :orientation)))
          (re-start-exif-jpg ()
            :report "jpg ZPB-EXIF:INVALID-EXIF-STREAM" ; how do I get this dynamically?
            (values nil nil nil))
          (re-start-jpg ()
            :report "jpg ZPB-EXIF:INVALID-JPEG-STREAM" ; how do I get this dynamically?
            (setf (file-signaled-error file-info) t)
            (values nil nil nil)))
        (values nil nil nil))))

(defmethod get-content-timestamp ((file-info file-info) exif-by-file) ; maybe convert this to defun and take 2 parameters then return timestamp and geo lat+lng
  "get filestamp for a file"
  (flet ((get-created-date (file-path)
           (let* ((stat (sb-posix:lstat file-path))
                  (parsed-date-info (get-parsed-date (make-instance 'date-info) (sb-posix:stat-mtime stat)))
                  (adjusted-year (+ 70 (date-year parsed-date-info)))) ;; the posix call is returning dates as 60 years earlier...
             (format nil "~d/~d/~d ~d:~d"
                     (date-month parsed-date-info) (date-day parsed-date-info) adjusted-year (date-hour parsed-date-info) (date-minute parsed-date-info)))))
    (let* ((file-path (file-path file-info))
           (extension (pathname-type file-path)))
      (if (or (string-equal "jpg" extension) (string-equal "jpeg" extension))
          (restart-case
              (let ((exif (funcall exif-by-file (file-path file-info))))
                (getf exif :date))
            (re-start-exif-jpg ()
              :report "jpg ZPB-EXIF:INVALID-EXIF-STREAM" ; how do I get this dynamically?
              (get-created-date file-path))
            (re-start-jpg ()
              :report "jpg ZPB-EXIF:INVALID-JPEG-STREAM" ; how do I get this dynamically?
              (setf (file-signaled-error file-info) t)
              (get-created-date file-path)))
          (get-created-date file-path)))))

(defun parse-file-for-exif (file-path)
  "use parse-exif-info and make-exif to get exif info
Examples: 
(let ((parsed-exif-info (parse-exif-data file-path)))
  (getf parsed-exif-info :date)
  (let ((exif (make-exif file-path)))
    (exif-value :DateTimeOriginal exif)))"
  ;; todo - if/when I get gps values, use (VALUES ...)
  (parse-exif-data file-path))

;; this is an example - it needs to be included in an orchestrator / imperative shell type function
(defun get-file-list (&optional (directory *content-root*))
  (handler-bind ((ZPB-EXIF:INVALID-EXIF-STREAM
                  (lambda (condition)
                    (declare (ignore condition))
                    (invoke-restart 're-start-exif-jpg)))
                 (ZPB-EXIF:INVALID-JPEG-STREAM
                  (lambda (condition)
                    (declare (ignore condition))
                    (invoke-restart 're-start-jpg)))) ;; invoke "emergency" function
    (flet ((process-file (file)
             (let ((exif-by-file (memoize #'(lambda (file-path) (parse-file-for-exif file-path)))))
               (multiple-value-bind (image-length image-width image-orientation)
                   (get-content-image-dimensions file exif-by-file)
                 (setf (file-timestamp file) (get-content-timestamp file exif-by-file)
                       (file-image-length file) image-length
                       (file-image-width file) image-width
                       (file-image-orientation file) image-orientation)))))
      (let ((content-files (get-content-files directory)))
        (mapc #'process-file (content-images content-files))
        (mapc #'process-file (content-videos content-files))
        content-files))))

(defun search-folders (search-path file-info &optional (index 0))
  "version of search that works with file-info list"
  (cond
    ((null file-info)
     nil)
    ((equal search-path (file-path (car file-info)))
     index)
    (t (search-folders search-path (cdr file-info) (+ 1 index)))))

(defun get-previous-folder-index (current-folder-index folders)
  (when (plusp current-folder-index)
    (let* ((current-folder-path (file-path (nth current-folder-index folders)))
           (current-folder (namestring current-folder-path))
           (parent-folder-end-position (position #\/ current-folder :from-end t :end (- (length current-folder) 1)))
           (previous-folder (truename (subseq current-folder 0 (+ 1 parent-folder-end-position)))))
      (or (search-folders previous-folder (cdr folders)) 0))))
