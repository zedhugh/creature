;; -*- coding: utf-8; lexical-binding: t; -*-

(autoload 'prettier-mode "prettier" "" t)

(defvar-local creature/formatter nil
  "Formatter for buffer.")

(defun creature/prettier-setup ()
  "Enable `prettier-mode' selectively."
  (when (and prettier-mode
             (or (not prettier-version) (prettier--in-node-modules-p)))
    (prettier-mode -1)))

(defun creature/prettier-json-parser ()
  (if (and buffer-file-name
           (seq-contains
            '("package.json" "package-lock.json" "composer.json")
            (file-name-nondirectory buffer-file-name)))
      '(json-stringify json json5)
    '(json json5 json-stringify)))

(defun creature/prettier-set-formatter ()
  (setq creature/formatter
        (if (bound-and-true-p prettier-mode)
            'creature/indent-region-or-buffer
          nil)))

(with-eval-after-load 'prettier
  (add-hook 'prettier-mode-hook #'creature/prettier-setup)
  (add-hook 'prettier-mode-hook #'creature/prettier-set-formatter)
  (add-to-list 'prettier-major-mode-parsers
               '(js-json-mode . creature/prettier-json-parser)))

(dolist (hook '(css-mode-hook
                js-base-mode-hook
                json-ts-mode-hook
                typescript-mode-hook
                typescript-ts-base-mode-hook))
  (add-hook hook #'prettier-mode))

(provide 'init-prettier)
