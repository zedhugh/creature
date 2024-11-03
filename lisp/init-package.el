;; -*- coding: utf-8; lexical-binding: t; -*-

(defconst creature/pkg-dir
  (expand-file-name
   "../site-lisp"
   (file-name-directory(or load-file-name buffer-file-name)))
  "Package directory.")

(require 'cl-lib)
(require 'subr-x)

(defun add-dir-and-subdirs-to-load-path (dir)
  "Add directory and its subdirectoris to `load-path'.
Detect whether there are any loadable module in DIR, if so, add DIR to `load-path'.
Do this recursively for subdirectories of DIR."
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))

  (let ((subdirs nil)
        (files nil)
        (temp-filepath nil)
        ;; (loadable-suffixes (get-load-suffixes))
        (load-extension (mapcar (lambda (str) (string-remove-prefix "." str)) load-suffixes))
        (exclude-dirs '("." ".."
                        "dist" "node_modules" "__pycache__" "test"
                        "RCS" "CVS" "rcs" "cvs" ".git" ".github")))

    (dolist (filename (directory-files dir))
      (setq temp-filepath (file-name-concat dir filename))
      (if (file-directory-p temp-filepath)
          (unless (member filename exclude-dirs)
            (add-to-list 'subdirs temp-filepath t))
        (when (file-regular-p temp-filepath)
          (add-to-list 'files temp-filepath t))))

    (when (cl-some (lambda (file)
                     (member (file-name-extension file) load-extension))
                   files)
      (add-to-list 'load-path dir t))
    (mapc #'add-dir-and-subdirs-to-load-path subdirs)))

(defun add-pkg-in-pkg-dir (pkg)
  "Add PKG located in `creature/pkg-dir' to `load-path'."
  (add-dir-and-subdirs-to-load-path (expand-file-name pkg creature/pkg-dir)))

(add-to-list 'load-path creature/pkg-dir t)

;; add basic packages
(add-pkg-in-pkg-dir "lazy-load")

;; builtin maybe exist
;; "compat" "transient"
(dolist (pkg '("compat" "transient"))
  (unless (locate-library pkg)
    (add-pkg-in-pkg-dir pkg)))

(provide 'init-package)
