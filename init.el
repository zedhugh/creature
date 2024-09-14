;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar creature/gc-cons-threshold
  (if (display-graphic-p) (* 64 1024 1024) (* 16 1024 1024))
  "The default value to use for `gc-cons-threshold'.
If freezing sometimes, decrease it. If stuttering, increase it.")

(defvar creature/gc-cons-upper-limit
  (if (display-graphic-p) (* 512 1024 1024) (* 128 1024 1024))
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar creature/gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbage collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist
  "Temporary `file-name-handler-alist' for restore after startup.")

;; Speed up startup
(defun creature/enlarge-gc-cons-threshold ()
  "Enlarge garbage collection threshold."
  (setq gc-cons-percentage  0.6
        gc-cons-threshold   creature/gc-cons-upper-limit))

(defun creature/normalize-gc-cons-threshold ()
  "Normalize garbage collection threshold."
  (setq gc-cons-percentage  0.1
        gc-cons-threshold   creature/gc-cons-threshold))

(creature/enlarge-gc-cons-threshold)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (creature/normalize-gc-cons-threshold)
            (setq file-name-handler-alist default-file-name-handler-alist)

            (setq read-process-output-max (* 1024 1024))

            ;; GC automatically when unfocused
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state) (garbage-collect))))
              (add-hook 'focus-out-hook #'garbage-collect))

            ;; Avoid GC while minibuffer active.
            (add-hook 'minibuffer-setup-hook
                      #'creature/enlarge-gc-cons-threshold)
            (add-hook 'minibuffer-exit-hook
                      #'creature/normalize-gc-cons-threshold)))

(defconst creature/config-dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "Root directory of Creature.")

(defconst creature/cache-dir
  (expand-file-name ".cache" creature/config-dir)
  "Cache directory of Creature.")

(defconst creature/prefix-key "M-m"
  "Prefix key for custom keybindings.")

(define-prefix-command 'creature/map)
(global-set-key (kbd creature/prefix-key) 'creature/map)

(defconst creature/pkg-dir
  (expand-file-name "site-lisp" creature/config-dir)
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
                        "dist" "node_modules" "__pycache__"
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

(add-to-list 'load-path (expand-file-name "lisp" creature/config-dir) t)
(add-dir-and-subdirs-to-load-path creature/pkg-dir)


(require 'init-built-in)
(require 'init-treesit)
(require 'init-modeline)
(require 'init-git)
(require 'init-awesome-pair)
(require 'init-theme)
(require 'init-vertico)
(require 'init-keybindings)
(require 'init-rime)
(require 'init-sdcv)
(require 'init-yasnippet)
(require 'init-ibuffer)
(require 'init-which-key)
(require 'init-editorconfig)
(require 'init-typescript)
(require 'init-eglot)
(require 'init-prettier)
(require 'init-emmet)
(require 'init-flymake)
(require 'init-org)
(require 'init-file)
(require 'init-proxy)
(require 'init-irc)
(require 'init-mail)
(require 'init-ace-window)
(require 'init-markdown)
(require 'init-media)
(require 'init-plantuml)


(setq custom-file (expand-file-name "custom.el" creature/cache-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))


(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/")))


(setq warning-minimum-level :error)
(setq warning-minimum-log-level :error)
