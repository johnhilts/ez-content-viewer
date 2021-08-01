(in-package #:content-viewer)

(define-info-class file path timestamp content-type alias-path)
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
                      (file (make-instance 'file-info)))
                 (populate-info-object file path timestamp content-type alias-path))))
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
