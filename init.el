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

(add-to-list 'load-path (expand-file-name "lisp" creature/config-dir) t)


(require 'init-modeline)
(when (eq system-type 'android)
  (require 'init-android))
(require 'init-built-in)
(require 'init-treesit)
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
(require 'init-prisma)
(require 'init-gptel)
(require 'init-nov)


(setq custom-file (expand-file-name "custom.el" creature/cache-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))


(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/")))


(setq warning-minimum-level :error)
(setq warning-minimum-log-level :error)
