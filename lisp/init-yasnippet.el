;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "yasnippet")
(add-pkg-in-pkg-dir "yasnippet-snippets")
(add-pkg-in-pkg-dir "auto-yasnippet")

(require 'lazy-load)
(require 'yasnippet)
(require 'yasnippet-snippets)


(defun fix-lisp-comment-snippets ()
  "Fix strange behavior about comment snippets in lisp."
  (setq-local comment-start ";;"))

(add-hook 'emacs-lisp-mode-hook #'fix-lisp-comment-snippets)

(add-hook 'prog-mode-hook #'yas-minor-mode)


(lazy-load-global-keys
 '(("w"   . aya-create)
   ("TAB" . aya-expand)
   ("SPC" . aya-expand-from-history)
   ("d"   . aya-delete-from-history)
   ("c"   . aya-clear-history)
   ("n"   . aya-next-in-history)
   ("p"   . aya-previous-in-history)
   ("s"   . aya-persist-snippet)
   ("o"   . aya-open-line))
 "auto-yasnippet" "C-c C-y")


(provide 'init-yasnippet)
