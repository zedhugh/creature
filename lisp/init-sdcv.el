;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "sdcv")
(add-pkg-in-pkg-dir "posframe")

(require 'lazy-load)

(require 'init-keybindings)

(autoload 'sdcv-say-word "sdcv" "")
(defun sdcv-say-word-dwim ()
  "If region active, say the region text, otherwise say the word at point."
  (interactive)
  (sdcv-say-word
   (if (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (word-at-point t))))

(lazy-load-local-keys
 '(("s" . sdcv-search-pointer)
   ("p" . sdcv-say-word-dwim))
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
          "朗道汉英字典5.0"))
  (lazy-load-set-keys
   '(("P" . sdcv-say-word-dwim))
   sdcv-mode-map))


(provide 'init-sdcv)
