;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "eslint-disable-rule")
(add-pkg-in-pkg-dir "emacs-flymake-eslint")

(defun creature/flymake-show-diagnostics-buffer-and-jump ()
  (interactive)
  (flymake-show-diagnostics-buffer)
  (select-window
   (get-buffer-window (flymake--diagnostics-buffer-name))))

(defcustom flymake-disabled-for-js/ts-in-dirnames nil
  "Disable `flymake-mode' in js/ts file in these directories."
  :type '(repeat string))

(defun creature/file-in-dir (filepath dirname)
  (and filepath
       (seq-contains-p (file-name-split filepath) dirname)))

(defun creature/flymake-need-disabled ()
  (and flymake-mode
       (derived-mode-p 'js-base-mode 'typescript-ts-base-mode 'typescript-mode)
       (cl-some (lambda (dir)
                  (creature/file-in-dir buffer-file-name dir))
                flymake-disabled-for-js/ts-in-dirnames)))

(defun creature/flymake-disable-in-compiled-js/ts-file ()
  (when (creature/flymake-need-disabled)
    (flymake-mode -1)))

(with-eval-after-load 'flymake
  (add-hook 'flymake-mode-hook #'creature/flymake-disable-in-compiled-js/ts-file)
  (add-hook 'flymake-mode-hook #'creature/flymake-add-eslint-backend)

  (lazy-load-set-keys
   '(("C-c C-n" . flymake-goto-next-error)
     ("C-c C-p" . flymake-goto-prev-error))
   flymake-mode-map)

  (require 'init-keybindings)
  (lazy-load-set-keys
   '(("ex" . flymake-show-diagnostic)
     ("el" . creature/flymake-show-diagnostics-buffer-and-jump))
   flymake-mode-map creature/prefix-key))


(autoload 'eslint-disable-rule-disable-next-line "eslint-disable-rule" "" t)
(autoload 'eslint-disable-rule-flymake "eslint-disable-rule-flymake")
(autoload 'eslint-disable-rule-flycheck "eslint-disable-rule-flycheck")

(autoload 'emacs-flymake-eslint-enable "emacs-flymake-eslint" "" t)
(autoload 'emacs-flymake-eslint--checker "emacs-flymake-eslint" "" t)

(defun eslint-disable-rule--emacs-flymake-eslint-active-p ()
  "Return non nil if `flymake-eslint' is enabled in the current buffer."
  (and
   (featurep 'flymake)
   (or (featurep 'flymake-eslint)
       (featurep 'emacs-flymake-eslint))
   flymake-mode
   (member eslint-disable-rule-flymake--checker-fn flymake-diagnostic-functions)))

(with-eval-after-load 'eslint-disable-rule-flymake
  (setq eslint-disable-rule-flymake--checker-fn #'emacs-flymake-eslint--checker)
  (advice-add 'eslint-disable-rule-flymake--eslint-active-p
              :override #'eslint-disable-rule--emacs-flymake-eslint-active-p))

(defun creature/flymake-add-eslint-backend ()
  (when (and flymake-mode
             (not (derived-mode-p 'js-json-mode 'json-mode 'json-ts-mode))
             (derived-mode-p 'js-base-mode
                             'typescript-mode
                             'typescript-ts-base-mode
                             'web-mode))
    (emacs-flymake-eslint-enable)))


(provide 'init-flymake)
