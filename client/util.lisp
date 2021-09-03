(in-package #:content-viewer)

(defun share-server-side-constants ()
  "feed server side constants to parenscript"
  (ps
    (defmacro share-server-side-constants ()
      (flet (
             (a-defvar (e) (equal 'defvar (car e)))
             (constants-from-server ()
               (read-complete-file-by-line "./common/constants.lisp")))
        `(progn
           ,@(mapcar #'print
              (remove-if-not #'a-defvar
                             (constants-from-server))))))

    (share-server-side-constants)))

(define-for-ps get-next-index (favorite-list)
  "calculate next index for favorite list"
  (let ((id-list (chain favorite-list (map #'(lambda (favorite) (@ favorite id)))))
        (max-fn (@ -Math max)))
    (if (length id-list)
        (+ 1 (chain max-fn (apply null id-list)))
        1)))
