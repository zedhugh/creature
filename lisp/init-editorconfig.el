;; -*- coding: utf-8; lexical-binding: t; -*-

(autoload 'editorconfig-mode "editorconfig" "" t)

(setq creature/editorconfig-timer
      (run-with-idle-timer 5 nil #'editorconfig-mode))

(add-hook 'find-file-hook #'editorconfig-mode)

(with-eval-after-load 'editorconfig
  (remove-hook 'find-file-hook #'editorconfig-mode)
  (cancel-timer creature/editorconfig-timer)
  (makunbound 'creature/editorconfig-timer))


(provide 'init-editorconfig)
