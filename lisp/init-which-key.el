;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'which-key)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)
(which-key-mode 1)

(which-key-add-keymap-based-replacements
  creature/map
  "b"  "buffer"
  "c"  "comments"
  "f"  "files"
  "g"  "magit"
  "gf" "magit files"
  "h"  "help"
  "q"  "quit option"
  "j"  "jump")


(provide 'init-which-key)
