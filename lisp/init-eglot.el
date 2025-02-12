;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)

(defun creature/eglot-load-markdown-for-doc ()
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (not (featurep 'markdown-mode))
             (locate-library "markdown-mode"))
    (require 'markdown-mode)))

(defun creature/eglot-hover-eldoc-function (cb)
  "Multiple line version of `eglot-hover-eldoc-function'."
  (when (eglot-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn (eglot--lambda ((Hover) contents range)
                     (eglot--when-buffer-window buf
                       (let ((info (unless (seq-empty-p contents)
                                     (eglot--hover-info contents range))))
                         (funcall cb info :buffer t))))
       :deferred :textDocument/hover))
    t))

(defun creature/eglot-format ()
  (interactive)
  (if creature/formatter
      (funcall creature/formatter)
    (eglot-format)))

(defun creature/disable-eglot-for-json ()
  "JSON mode is derived from `js-mode', but json file don't need `eglot'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eglot-managed-p)
                 (derived-mode-p 'js-json-mode 'json-mode 'json-ts-mode))
        (run-with-timer 0 nil
                        (lambda ()
                          (with-current-buffer buffer
                            (flymake-mode -1)
                            (eglot--managed-mode-off))))))))

(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

  (add-hook 'eglot-managed-mode-hook #'creature/eglot-load-markdown-for-doc)
  (add-hook 'eglot-managed-mode-hook #'creature/disable-eglot-for-json)

  ;; variable `eglot-prefer-plaintext' invoked in eglot-1.14
  ;; and document only show one line by eldoc since eglot-1.14
  (when (boundp 'eglot-prefer-plaintext)
    (advice-add 'eglot-hover-eldoc-function :override #'creature/eglot-hover-eldoc-function))

  (lazy-load-set-keys
   '(("M-." . xref-find-definitions)
     ("M-?" . xref-find-references)
     ("C-c r" . eglot-rename)
     ("C-c o" . eglot-code-actions)
     ("C-M-\\" . creature/eglot-format))
   eglot-mode-map))


(dolist (hook '(css-mode-hook
                js-base-mode-hook
                typescript-mode-hook
                typescript-ts-base-mode-hook
                sh-mode-hook
                bash-ts-mode-hook
                python-mode-hook
                python-ts-mode-hook
                cmake-mode-hook
                c-mode-hook
                c++-mode-hook
                c-ts-base-mode-hook))
  (add-hook hook #'eglot-ensure))


(provide 'init-eglot)
