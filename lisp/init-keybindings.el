;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)

;;; expand-region
(lazy-load-local-keys
 '(("v" . er/expand-region))
 creature/map "expand-region")


;;; rg
(lazy-load-global-keys
 '(("C-c s" . rg-menu))
 "rg")


;;; avy
(lazy-load-local-keys
 '(("jl" . avy-goto-line)
   ("jw" . avy-goto-word-1))
 creature/map "avy")


;;; mwim
(lazy-load-global-keys
 '(("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line))
 "mwim")


;;; symbol-overlay
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


(require 'init-func)
(lazy-load-set-keys
 '(("C-x k" . kill-this-buffer)
   ("C-x K" . kill-buffer)
   ("C-M-\\" . creature/indent-region-or-buffer)
   ("DEL" . backward-delete-char-untabify)))

(setq backward-delete-char-untabify-method 'hungry)


(lazy-load-set-keys
 '(("bb" . switch-to-buffer)
   ("bd" . kill-current-buffer)
   ("be" . eval-buffer)

   ("fi" . creature/open-init-file)
   ("fo" . creature/open-file-or-directory-in-external-app)
   ("fj" . dired-jump)
   ("ff" . find-file)
   ("fp" . find-file-at-point)
   ("fs" . save-buffer)

   ("hf" . describe-function)
   ("hF" . describe-face)
   ("hv" . describe-variable)
   ("hk" . describe-key)
   ("ho" . describe-symbol)
   ("hp" . describe-package)
   ("qk" . save-buffers-kill-emacs)
   ("qq" . save-buffers-kill-terminal)

   ("w" . creature/transient-window)
   ("SPC" . execute-extended-command))
 creature/map)


(require 'transient)
(transient-define-prefix creature/transient-window ()
  "Show menu buffer for window operations."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  [["Jump"
    ("d" "Window down" windmove-down)
    ("u" "Window up" windmove-up)
    ("l" "Window left" windmove-left)
    ("r" "Window right" windmove-right)
    ("p" "Window Previous" (lambda ()
                             (interactive)
                             (other-window -1))
     )
    ("n" "Window next" other-window)]

   ["Size reset"
    ("{" "Height shrink" shrink-window)
    ("}" "Height enlarge" enlarge-window)
    ("[" "Width shrink" shrink-window-horizontally)
    ("]" "Width enlarge" enlarge-window-horizontally)
    ("=" "Banlance" balance-windows :transient nil)]

   ["Operation"
    ("-" "Split below" split-window-below)
    ("/" "Split right" split-window-right)
    ("m" "Delete Others" delete-other-windows :transient nil)
    ("k" "Delete window" delete-window)
    ]])


(provide 'init-keybindings)
