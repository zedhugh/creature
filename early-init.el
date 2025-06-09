;; -*- coding: utf-8; lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

;; Initialize frame with alpha
(add-to-list 'default-frame-alist '(alpha-background . 85))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


(defun creature/welcom-message ()
  (let (init-time startup-time)
    (setq startup-time (time-since before-init-time))
    (setq init-time
          (time-subtract after-init-time before-init-time))
    (setq initial-scratch-message
          (format ";; startup: %fs -- init: %fs\n\
;; Happy hacking %s - Emacs loves you.\n\n"
                  (float-time startup-time)
                  (float-time init-time)
                  (or (user-login-name) "user"))))

  (with-current-buffer "*scratch*"
    (erase-buffer)
    (insert initial-scratch-message)
    (set-buffer-modified-p nil)))

(add-hook 'emacs-startup-hook #'creature/welcom-message 1999)
