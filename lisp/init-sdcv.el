;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "sdcv")
(add-pkg-in-pkg-dir "posframe")

(require 'lazy-load)


(require 'init-keybindings)
(lazy-load-local-keys
 '(("s" . sdcv-search-pointer))
 creature/map "sdcv")

(with-eval-after-load 'sdcv
  (setq sdcv-dictionary-data-dir (file-truename "~/.stardict/dic/"))
  (setq sdcv-dictionary-simple-list
        '("牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"))
  (setq sdcv-dictionary-complete-list
        '("牛津英汉双解美化版"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0")))


(provide 'init-sdcv)
