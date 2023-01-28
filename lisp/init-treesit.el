;; -*- coding: utf-8; lexical-binding: t; -*-

(defconst creature/treesit-available
  (and (fboundp 'treesit-available-p) (treesit-available-p))
  "Built-in `treesit' available.")

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (unless (featurep 'treesit) (require 'treesit))
  (when (treesit-ready-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))))


(provide 'init-treesit)
