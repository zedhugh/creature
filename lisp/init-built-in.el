;; -*- coding: utf-8; lexical-binding: t; -*-

;; Encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Don't show prompt when call function
(fset 'yes-or-no-p 'y-or-n-p)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disable nil)
(put 'narrow-to-region 'disabled nil)

;; Always use space for indentation
(setq-default tab-width         4)
(setq-default indent-tabs-mode  nil)

;; Highlight trail whitespace
(setq show-trailing-whitespace t)

;; Save minibuffer history
(savehist-mode)
(setq history-length                1000
      savehist-autosave-interval    60
      enable-recursive-minibuffers  t
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history))

;; Record cursor position for file
(save-place-mode 1)

;; Delete file directly
(setq delete-by-moving-to-trash t)

;; No backup file
(setq make-backup-files nil)

(global-auto-revert-mode 1)

;; Auto save file
(setq auto-save-default t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(setq creature/recentf-mode-timer
      (run-with-idle-timer 5 nil #'recentf-mode))

(add-hook 'find-file-hook #'recentf-mode)

(with-eval-after-load 'recentf
  (remove-hook 'find-file-hook #'recentf-mode)
  (cancel-timer creature/recentf-mode-timer)
  (makunbound 'creature/recentf-mode-timer)

  (setq recentf-max-saved-items 1000)
  ;; (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "node_modules")
  (add-to-list 'recentf-exclude "site-lisp")
  (add-to-list 'recentf-exclude "\\.gpg\\'")

  (with-eval-after-load 'package
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))))


;; Disable bell
(setq visible-bell          nil
      ring-bell-function    'ignore)

;; Keep cursor at end of lines when prev
;; position of cursor is at the end.
;; Require line-move-visual is nil.
;; (setq track-eol         t
;;       line-move-visual  nil)

;; Do not show startup screen
(setq inhibit-splash-screen     t       ; no startup screen
      x-gtk-use-system-tooltips nil)    ; no gtk tooltips

(electric-pair-mode 1)
(electric-indent-mode 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(with-eval-after-load 'dired
  (require 'dired-x)

  (setq dired-dwim-target t)

  ;; show file size human readable
  (setq dired-listing-switches "-aghG")

  ;; copy and delete directory recursive
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  ;; don't make too many dired buffer
  (put 'dired-find-alternate-file 'disabled nil))

(add-hook 'display-fill-column-indicator-mode-hook
          (lambda ()
            (setq fill-column 80)))

(add-hook 'c-mode-common-hook #'display-fill-column-indicator-mode)

(unless (featurep 'server) (require 'server))
(unless (server-running-p) (server-start))


(provide 'init-built-in)
