;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)

(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-autoshutdown t)

  (lazy-load-set-keys
   '(("M-." . xref-find-definitions)
     ("M-?" . xref-find-references)
     ("C-c r" . eglot-rename)
     ("C-c o" . eglot-code-actions))
   eglot-mode-map))

(dolist (hook '(css-mode-hook js-mode-hook typescript-mode-hook))
  (add-hook hook #'eglot-ensure))


(provide 'init-eglot)
