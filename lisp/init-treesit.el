;; -*- coding: utf-8; lexical-binding: t; -*-

(defconst creature/treesit-available
  (and (fboundp 'treesit-available-p) (treesit-available-p))
  "Built-in `treesit' available.")

(when creature/treesit-available
  (unless (featurep 'treesit) (require 'treesit))
  (when (treesit-ready-p 'javascript)
    (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode)))

  (when (treesit-ready-p 'c)
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))
  (when (treesit-ready-p 'cpp)
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

  (when (treesit-ready-p 'cmake)
    (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))))


(provide 'init-treesit)
