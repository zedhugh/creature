;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "eslint-disable-rule")
(add-pkg-in-pkg-dir "eslint")

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

(autoload 'eslint-flymake-enable "eslint-flymake" "" t)
(autoload 'eslint-flymake-backend "eslint-flymake" "" t)

(defun eslint-disable-rule--eslint-flymake-active-p ()
  "Return non nil if `flymake-eslint' is enabled in the current buffer."
  (and
   (featurep 'flymake)
   (or (featurep 'flymake-eslint)
       (featurep 'eslint-flymake))
   flymake-mode
   (member eslint-disable-rule-flymake--checker-fn flymake-diagnostic-functions)))

(with-eval-after-load 'eslint-disable-rule-flymake
  (setq eslint-disable-rule-flymake--checker-fn #'eslint-flymake-backend)
  (advice-add 'eslint-disable-rule-flymake--eslint-active-p
              :override #'eslint-disable-rule--eslint-flymake-active-p))

(defun creature/flymake-add-eslint-backend ()
  (when (and flymake-mode
             (not (derived-mode-p 'js-json-mode 'json-mode 'json-ts-mode))
             (derived-mode-p 'js-base-mode
                             'typescript-mode
                             'typescript-ts-base-mode
                             'web-mode))
    (eslint-flymake-enable)))


(provide 'init-flymake)
