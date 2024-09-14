;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "emmet-mode")

(autoload 'emmet-mode "emmet-mode" "" t)

(with-eval-after-load 'emmet-mode
  (setq emmet-self-closing-tag-style " /")

  (dolist (mode '(js-jsx-mode typescript-tsx-mode tsx-ts-mode))
    (add-to-list 'emmet-jsx-major-modes mode)))

(dolist (hook '(css-mode-hook
                web-mode-hook
                html-mode-hook
                js-jsx-mode-hook
                tsx-ts-mode-hook
                typescript-tsx-mode-hook))
  (add-hook hook #'emmet-mode))


(provide 'init-emmet)
