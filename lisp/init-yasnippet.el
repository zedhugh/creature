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

(lazy-load-local-keys
 '(("C-M-i" . yas-insert-snippet))
 yas-minor-mode-map "yasnippet")


(defvar creature/company-with-yasnippet t
  "Make every `company-backend' with `company-yasnippet'.")

(defun creature/add-snippet (backend)
  "Add `company-yasnippet' to echo backend in `company-backends'."
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun creature/handle-company-backends ()
  "Handle `company-yasnippet' in `company-backends'."
  (set (make-local-variable 'company-backends)
       (mapcar #'creature/add-snippet company-backends)))

(defun creature/yasnippet-setup ()
  "Setup yasnippet."
  (when (and (bound-and-true-p company-mode) creature/company-with-yasnippet)
    (with-current-buffer (buffer-name)
      (run-with-timer 0.5 nil #'creature/handle-company-backends))))

(with-eval-after-load 'yasnippet
  ;; (setq yas-indent-line 'auto)       ;don't indent for snippet
  (add-hook 'yas-minor-mode-hook #'creature/yasnippet-setup))

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
