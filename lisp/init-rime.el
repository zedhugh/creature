;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)
(require 'rime)


(setq default-input-method "rime")
(setq rime-show-candidate 'posframe)
(setq rime-posframe-style 'simple)
(setq rime-posframe-fixed-position t)

(lazy-load-set-keys
 '(("M-i" . rime-select-schema))
 rime-mode-map)

(lazy-load-set-keys
 '(("M-i" . rime-inline-ascii)
   ("M-o" . rime--backspace)
   ("M-h" . rime--escape)
   ("C-S-<return>" . rime-send-keybinding))
 rime-active-mode-map)


(provide 'init-rime)
