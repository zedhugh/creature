;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)


(with-eval-after-load 'ace-window
  (setq aw-dispatch-when-more-than 3))

(defun creature/auto-toggle-ace-window-posframe-mode (&rest _ignore)
  "Enable `ace-window-posframe-mode' when Emacs running with GUI, otherwise disable it."
  (if (display-graphic-p)
      (progn
        (require 'ace-window-posframe)
        (ace-window-posframe-mode 1))
    (when (fboundp 'ace-window-posframe-mode)
      (ace-window-posframe-mode -1))))

(advice-add 'ace-window :before #'creature/auto-toggle-ace-window-posframe-mode)

(lazy-load-global-keys
 '(("C-x o" . ace-window))
 "ace-window")


(provide 'init-ace-window)
