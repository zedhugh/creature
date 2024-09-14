;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "plantuml-mode")
(add-pkg-in-pkg-dir "dash")

(autoload 'plantuml-mode "plantuml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.\\(plantuml\\|pum\\|plu\\)\\'" . plantuml-mode))

(defun plantuml-completion-at-point ()
  "Function used for `completion-at-point-functions' in `plantuml-mode'.
Copy from https://github.com/skuro/plantuml-mode/issues/98."
  (let ((completion-ignore-case t)
        (bounds (bounds-of-thing-at-point 'symbol))
        (keywords plantuml-kwdList))
    (when (and bounds keywords)
      (list (car bounds)
            (cdr bounds)
            keywords
            :exclusive 'no
            :company-docsig #'indentity))))

(with-eval-after-load 'plantuml-mode
  (setq plantuml-exec-mode 'executable
        plantuml-default-exec-mode 'executable)
  (add-hook 'plantuml-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'plantuml-completion-at-point nil 'local))))


(provide 'init-plantuml)
