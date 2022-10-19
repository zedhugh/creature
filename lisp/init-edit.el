;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)

;;; symbol-overlay-map
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
(lazy-load-global-keys
 '(("s-i" . symbol-overlay-put)
   ("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next))
 "symbol-overlay")


;;; mwim
(lazy-load-global-keys
 '(("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line))
 "mwim")


(setq backward-delete-char-untabify-method 'hungry)


(provide 'init-edit)
