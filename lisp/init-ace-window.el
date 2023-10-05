;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)


(with-eval-after-load 'ace-window
  (require 'ace-window-posframe)
  (ace-window-posframe-mode 1)
  (setq aw-dispatch-when-more-than 3))

(lazy-load-global-keys
 '(("C-x o" . ace-window))
 "ace-window")


(provide 'init-ace-window)
