;; -*- coding: utf-8; lexical-binding: t; -*-

(autoload 'prettier-mode "prettier" "" t)

(defun creature/prettier-setup ()
  "Enable ‘prettier-mode’ selectively."
  (when (and prettier-mode
             (or (not prettier-version) (prettier--in-node-modules-p)))
    (prettier-mode -1)))

(with-eval-after-load 'prettier
  (add-hook 'prettier-mode-hook #'creature/prettier-setup))

(dolist (hook '(css-mode-hook js-mode-hook typescript-mode-hook))
  (add-hook hook #'prettier-mode))

(provide 'init-prettier)
