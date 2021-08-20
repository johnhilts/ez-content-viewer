(in-package #:content-viewer)

;; (defun client-file ()
;;   "define client side functions to handle files"
;;   ;; (ps 
;;   ;;   (defvar todo-list ([])))
;;   t)

(ps
  (defmacro with-callback (fn &body body)
    `(,(car fn) ,@(cdr fn) #'(lambda (),@body))))

(define-for-ps delete-file-on-server (delete-url-object call-back)
  "save updated todo on server"
  (send-to-server *file-api-endpoint* "DELETE" delete-url-object call-back))
    
(define-for-ps delete-file-by-url (delete-url)
  "delete file on client and server and re-render html elements"
  (delete-file-on-server (create file-path delete-url))
  t)
    
