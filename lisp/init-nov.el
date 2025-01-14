;; -*- coding: utf-8; lexical-binding: t; -*-

;; `nov' package need enable libxml2 for Emacs

(add-pkg-in-pkg-dir "nov")
(add-pkg-in-pkg-dir "esxml")
(add-pkg-in-pkg-dir "emacs-kv")
(add-pkg-in-pkg-dir "emacs-db")

(autoload 'nov-mode "nov" "" t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(provide 'init-nov)
