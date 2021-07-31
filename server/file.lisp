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
