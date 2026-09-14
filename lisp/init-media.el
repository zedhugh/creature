;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)

(creature/pkg-active 'emms)

(with-eval-after-load 'emms-lyrics
  (setq emms-lyrics-dir "/home/zedhugh/.lyrics"))

(with-eval-after-load 'emms-source-file
  (setq emms-source-file-default-directory "/home/zedhugh/music"))

(defun creature/emms-setup ()
  (interactive)
  (emms-all)
  (emms-default-players))


(provide 'init-media)
