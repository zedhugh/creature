;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)

(defun creature/eglot-load-markdown-for-doc ()
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (not (featurep 'markdown-mode))
             (locate-library "markdown-mode"))
    (require 'markdown-mode)))

(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

  (add-hook 'eglot-managed-mode-hook #'creature/eglot-load-markdown-for-doc)

  (lazy-load-set-keys
   '(("M-." . xref-find-definitions)
     ("M-?" . xref-find-references)
     ("C-c r" . eglot-rename)
     ("C-c o" . eglot-code-actions))
   eglot-mode-map))


;; json mode is derived from `js-mode', but json file don't need `eglot'
(add-hook 'js-base-mode-hook
          (lambda ()
            (unless (derived-mode-p 'js-json-mode 'json-mode)
              (eglot-ensure))))

(dolist (hook '(css-mode-hook
                typescript-mode-hook
                typescript-ts-base-mode-hook
                sh-mode-hook
                bash-ts-mode
                cmake-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook #'eglot-ensure))


(provide 'init-eglot)
