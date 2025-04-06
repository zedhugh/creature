;; -*- coding: utf-8; lexical-binding: t; -*-

;; Make command install by `termux' accessible
(let ((termux-path "/data/data/com.termux/files/usr/bin"))
  (setenv "PATH" (format "%s:%s" termux-path (getenv "PATH")))
  (add-to-list 'exec-path termux-path))


(provide 'init-android)
