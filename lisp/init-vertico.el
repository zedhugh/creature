;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "marginalia")
(add-pkg-in-pkg-dir "vertico")
(add-pkg-in-pkg-dir "corfu")
(add-pkg-in-pkg-dir "orderless")
(add-pkg-in-pkg-dir "corfu-doc")
(add-pkg-in-pkg-dir "cape")
(add-pkg-in-pkg-dir "consult")
(add-pkg-in-pkg-dir "emacs-corfu-terminal")
(add-pkg-in-pkg-dir "emacs-popon")
(add-pkg-in-pkg-dir "emacs-corfu-doc-terminal")
(add-pkg-in-pkg-dir "embark")


(require 'marginalia)
(marginalia-mode 1)


(require 'vertico)
(vertico-mode 1)
(require 'vertico-sort)
(setq vertico-sort-function 'vertico-sort-history-length-alpha)


(require 'corfu)
(global-corfu-mode 'toggle)
(setq corfu-auto t
      corfu-cycle t
      corfu-auto-delay 0.05
      corfu-auto-prefix 2
      corfu-quit-no-match t)

(autoload 'corfu-info-documentation "corfu-info" "" t)
(autoload 'corfu-info-location "corfu-info" "" t)

(require 'orderless)
(setq completion-styles '(orderless basic partial-completion flex)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'corfu-doc)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)


;;; indexed extension
(let ((start 0)
      (count 10))
  (require 'vertico-indexed)
  (require 'corfu-indexed)
  (vertico-indexed-mode 1)
  (corfu-indexed-mode 1)
  (setq vertico-count count
        vertico-indexed-start start
        corfu-count count
        corfu-indexed-start start)
  (dotimes (i count)
    (let ((press-key (format "M-%d" i))
          (target-key (format "C-u %d RET" i)))
      (define-key vertico-map (kbd press-key) (kbd target-key))
      (define-key corfu-map (kbd press-key) (kbd target-key)))))


;;; quick extension
(let ((quick1 "wesdf")
      (quick2 "iojkl")
      (trigger "M-SPC"))
  (with-eval-after-load 'vertico-quick
    (setq vertico-quick1 quick1)
    (setq vertico-quick2 quick2))

  (with-eval-after-load 'corfu-quick
    (setq corfu-quick1 quick1)
    (setq corfu-quick2 quick2))

  (lazy-load-local-keys
   `((,trigger . vertico-quick-insert))
   vertico-map
   "vertico-quick")

  (lazy-load-local-keys
   `((,trigger . corfu-quick-complete))
   corfu-map
   "corfu-quick"))


(lazy-load-global-keys
 '(("C-'" . cape-file))
 "cape")


(require 'init-keybindings)
(lazy-load-local-keys
 '(("fr" . consult-recent-file))
 creature/map "consult")
(with-eval-after-load 'consult
  (recentf-mode 1))


(defun creature/load-corfu-terminal-in-no-gui-env ()
  (if (or (display-graphic-p) (featurep 'tty-child-frames))
      (progn
        (require 'corfu-popupinfo)
        (corfu-popupinfo-mode 1))
    (progn
      (require 'corfu-terminal)
      (require 'corfu-doc-terminal)
      (corfu-terminal-mode 1)
      (corfu-doc-terminal-mode 1))))

(add-hook 'emacs-startup-hook #'creature/load-corfu-terminal-in-no-gui-env)
(add-hook 'server-after-make-frame-hook #'creature/load-corfu-terminal-in-no-gui-env)

(provide 'init-vertico)
