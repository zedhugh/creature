;; -*- coding: utf-8; lexical-binding: t; -*-

(autoload 'emmet-mode "emmet-mode" "" t)

(with-eval-after-load 'emmet-mode
  (setq emmet-self-closing-tag-style " /"))

(dolist (hook '(css-mode-hook
                web-mode-hook
                html-mode-hook
                js-jsx-mode-hook
                typescript-tsx-mode-hook))
  (add-hook hook #'emmet-mode))


(provide 'init-emmet)
