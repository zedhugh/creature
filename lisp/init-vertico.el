;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'marginalia)
(marginalia-mode 1)


(require 'vertico)
(vertico-mode 1)
(require 'vertico-indexed)
(vertico-indexed-mode 1)


(require 'corfu)
(global-corfu-mode 'toggle)
(setq corfu-auto t
      corfu-auto-delay 0.05
      corfu-auto-prefix 2
      corfu-quit-no-match t)

(require 'orderless)
(setq completion-styles '(orderless basic partial-completion flex)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'corfu-doc)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(lazy-load-local-keys
 '(("M-SPC" . corfu-quick-complete))
 corfu-map
 "corfu-quick")

(lazy-load-global-keys
 '(("C-'" . cape-file))
 "cape")


(lazy-load-local-keys
 '(("fr" . consult-recent-file))
 creature/map "consult")
(with-eval-after-load 'consult
  (recentf-mode 1))


(defun creature/load-corfu-terminal-in-no-gui-env ()
  (unless (display-graphic-p)
    (require 'corfu-terminal)
    (require 'corfu-doc-terminal)
    (corfu-terminal-mode 1)
    (corfu-doc-terminal-mode 1)))

(add-hook 'emacs-startup-hook #'creature/load-corfu-terminal-in-no-gui-env)
(add-hook 'server-after-make-frame-hook #'creature/load-corfu-terminal-in-no-gui-env)

(provide 'init-vertico)
