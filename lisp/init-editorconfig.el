;; -*- coding: utf-8; lexical-binding: t; -*-

(autoload 'editorconfig-mode "editorconfig" "" t)

(defun creature/editorconfig-setup ()
  (editorconfig-mode 1)
  (remove-hook 'find-file-hook #'creature/editorconfig-setup)
  (cancel-timer creature/editorconfig-timer)
  (makunbound 'creature/editorconfig-timer))

(setq creature/editorconfig-timer
      (run-with-idle-timer 5 nil #'creature/editorconfig-setup))

(add-hook 'find-file-hook #'creature/editorconfig-setup)


(provide 'init-editorconfig)
