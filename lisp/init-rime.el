;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "emacs-rime")
(add-pkg-in-pkg-dir "dash")
(add-pkg-in-pkg-dir "posframe")

(require 'lazy-load)
(require 'rime)


(setq default-input-method "rime")
(setq rime-show-candidate 'posframe)
(setq rime-posframe-style 'vertical)
(setq rime-posframe-fixed-position t)
(setq rime-sidewindow-style 'vertical)

(lazy-load-set-keys
 '(("M-i" . rime-select-schema)
   ("C-`" . rime-send-keybinding)
   ("M-`" . rime-send-keybinding)
   ("C-@" . rime-send-keybinding)
   ("M-SPC" . rime-send-keybinding))
 rime-mode-map)

(lazy-load-set-keys
 '(("M-i" . rime-inline-ascii)
   ("M-o" . rime--backspace)
   ("M-h" . rime--escape)
   ("C-S-<return>" . rime-send-keybinding))
 rime-active-mode-map)

(provide 'init-rime)
