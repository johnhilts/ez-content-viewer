(in-package #:content-viewer)

(defparameter *users-root-folder-path* "./users")
(defparameter *user-index* nil)

(defvar *file-api-endpoint*  "/file-data")
;; (defvar *app-settings-api-endpoint*  "/app-settings-data")
;; (defvar *app-settings-file-path* "./app-settings-list.sexp")
(defvar *favorite-api-endpoint*  "/favorite-data")
(defvar *favorite-file-path* "./favorite-list.sexp")
(defvar *web-settings-file-path* "./web-settings.sexp")

;; (defvar *recipe-api-endpoint*  "/recipe-data")
;; (defvar *recipe-file-path* "./recipe-list.sexp")

;; note: apparently defconstant not supported by parenscrpt
(defvar *orientation-rotation-minus-90* 6
  "orientation based on exif info")
