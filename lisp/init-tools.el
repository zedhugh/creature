;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)


(lazy-load-local-keys
 '(("s" . sdcv-search-pointer))
 creature/map "sdcv")

(with-eval-after-load 'sdcv
  (setq sdcv-dictionary-data-dir (file-truename "~/.stardict/dic/"))
  (setq sdcv-dictionary-simple-list
        '("牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"))
  (setq sdcv-dictionary-complete-list
        '("牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0")))


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


(require 'rime)

(setq default-input-method "rime")
(setq rime-show-candidate 'posframe)
(setq rime-posframe-style 'simple)
(setq rime-posframe-fixed-position t)
(setq rime-disable-predicates
      '(rime-predicate-prog-in-code-p
        rime-predicate-hydra-p))

(lazy-load-set-keys
 '(("M-i" . rime-force-enable))
 rime-mode-map)

(lazy-load-set-keys
 '(("M-i" . rime-inline-ascii)
   ("M-o" . rime--backspace)
   ("M-h" . rime--escape))
 rime-active-mode-map)


(lazy-load-local-keys
 '(("v" . er/expand-region))
 creature/map "expand-region")


(lazy-load-global-keys
 '(("C-c s" . rg-menu))
 "rg")


(provide 'init-tools)
