;; -*- coding: utf-8; lexical-binding: t; -*-

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

  (lazy-load-set-keys
   '(("ex" . flymake-show-diagnostic)
     ("el" . creature/flymake-show-diagnostics-buffer-and-jump))
   flymake-mode-map creature/prefix-key))


(autoload 'eslint-disable-rule-disable-next-line "eslint-disable-rule" "" t)
(autoload 'eslint-disable-rule-flymake "eslint-disable-rule-flymake")
(autoload 'eslint-disable-rule-flycheck "eslint-disable-rule-flycheck")


(defvar-local creature/eslint-inited nil
  "Eslint inited for buffer.")

(autoload 'flymake-eslint-enable "flymake-eslint" "" t)
(defun creature/flymake-add-eslint-backend ()
  (when (and flymake-mode
             (not creature/eslint-inited)
             (not (derived-mode-p 'js-json-mode 'json-mode 'json-ts-mode))
             (derived-mode-p 'js-base-mode
                             'typescript-mode
                             'typescript-ts-base-mode
                             'web-mode))

    ;; flymake-eslint-enable 内部会再次调用 (flymake-mode 1) ，为防止无限
    ;; 循环调用，用 ‘creature/eslint-inited’ 记录该函数调用状态避免多次调用
    (setq creature/eslint-inited t)

    (flymake-eslint-enable)
    (setq flymake-eslint-project-root
          (creature/flymake-eslint-find-work-dir))))

(defun creature/flymake-eslint-find-work-dir ()
  (let ((max-len 0)
        (curr-len 0)
        (temp-dir nil)
        (work-dir nil))
    (dolist (filename '(".eslintrc"
                        ".eslintrc.js"
                        ".eslintrc.cjs"
                        ".eslintrc.yaml"
                        ".eslintrc.yml"
                        ".eslintrc.json"
                        "package.json"))
      (setq temp-dir (locate-dominating-file buffer-file-name filename))
      (when (stringp temp-dir)
        (setq curr-len (string-bytes (file-truename temp-dir)))

        (when (> curr-len max-len)
          (setq max-len curr-len
                work-dir temp-dir))))
    work-dir))


(provide 'init-flymake)
