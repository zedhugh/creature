;; -*- coding: utf-8; lexical-binding: t; -*-

(autoload 'editorconfig-mode "editorconfig" "" t)

(defun creature/editorconfig-setup ()
  ;; 防止多次执行时因为定时器被清除而报错
  (when (and (boundp 'creature/editorconfig-timer)
             (timerp creature/editorconfig-timer))
    (editorconfig-mode 1)
    ;; 修复通过打开文件启动该插件时打开的文件缩进配置不对
    (editorconfig-mode-apply)
    (remove-hook 'find-file-hook #'creature/editorconfig-setup)
    (cancel-timer creature/editorconfig-timer)
    (makunbound 'creature/editorconfig-timer)))

(setq creature/editorconfig-timer
      (run-with-idle-timer 5 nil #'creature/editorconfig-setup))

(add-hook 'find-file-hook #'creature/editorconfig-setup)


(autoload 'editorconfig-conf-mode "editorconfig-conf-mode" "" t)
(add-to-list 'auto-mode-alist
             '("\\.editorconfig\\'" . editorconfig-conf-mode))


(provide 'init-editorconfig)
