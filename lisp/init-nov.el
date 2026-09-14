;; -*- coding: utf-8; lexical-binding: t; -*-

;; `nov' package need enable libxml2 for Emacs

(require 'init-package)

(creature/pkg-active 'nov)

(autoload 'nov-mode "nov" "" t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(provide 'init-nov)
