;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "vimrc-mode")
(add-pkg-in-pkg-dir "yaml-mode")
(add-pkg-in-pkg-dir "lua-mode")
(add-pkg-in-pkg-dir "nginx-mode")
(add-pkg-in-pkg-dir "pdf-tools")
(add-pkg-in-pkg-dir "tablist")
(add-pkg-in-pkg-dir "saveplace-pdf-view")
(add-pkg-in-pkg-dir "graphviz-dot-mode")
(add-pkg-in-pkg-dir "meson-mode")


(autoload 'vimrc-mode "vimrc-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))


(autoload 'yaml-mode "yaml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))
(add-to-list 'magic-mode-alist
             '("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode))


(autoload 'lua-mode "lua-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


(autoload 'nginx-mode "nginx-mode" "" t)
(add-to-list 'auto-mode-alist '("nginx\\.conf\\'"  . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/.+\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist
             '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))
(add-to-list
 'magic-fallback-mode-alist
 '("\\(?:.*\n\\)*\\(?:http\\|server\\|location .+\\|upstream .+\\)[ \n\t]+{"
   . nginx-mode))


(autoload 'pdf-tools-install "pdf-tools" "" t)
(autoload 'pdf-sync-minor-mode "pdf-sync" "" t)
(autoload 'pdf-annot-minor-mode "pdf-annot" "" t)
(autoload 'pdf-links-minor-mode "pdf-links" "" t)
(autoload 'pdf-history-minor-mode "pdf-history" "" t)
(autoload 'pdf-outline-minor-mode "pdf-outline" "" t)
(autoload 'pdf-occur-global-minor-mode "pdf-occur" "" t)
(require 'pdf-loader)
(pdf-loader-install t t t)

(with-eval-after-load 'pdf-view
  (require 'saveplace-pdf-view))
;; (with-eval-after-load 'pdf-view
;;   (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode))


;; (add-to-list 'auto-mode-alist '("package\\.env\\'"             . conf-mode))
;; (add-to-list 'auto-mode-alist '("package\\.use\\'"             . conf-mode))
;; (add-to-list 'auto-mode-alist '("package\\.mask\\'"            . conf-mode))
;; (add-to-list 'auto-mode-alist '("package\\.license\\'"         . conf-mode))
;; (add-to-list 'auto-mode-alist '("package\\.keywords\\'"        . conf-mode))
;; (add-to-list 'auto-mode-alist '("package\\.accept_keywords\\'" . conf-mode))
(add-to-list 'auto-mode-alist
             '("/\\(package\\.\\(mask\\|unmask\\|use\\|env\
\\|license\\|properties\\|accept_\\(keywords\\|restrict\\)\\)\
\\|\\(package\\.\\)?use.\\(stable\\.\\)?\\(force\\|mask\\)\\)\\'"
               . conf-space-mode))
(add-to-list 'auto-mode-alist
             '("/package\\.\\(mask\\|unmask\\|use\\|env\
\\|accept_\\(keywords\\|restrict\\)\\)/.*\\'"
               . conf-space-mode))

(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js-mode))


(autoload 'cmake-mode "cmake-mode" "" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))


(autoload 'meson-mode "meson-mode" "" t)
(add-to-list
 'auto-mode-alist
 '("/meson\\(\\.build\\|_options\\.txt\\|\\.options\\)\\'" . meson-mode))


;; optimize performance of long-line files
(setq-default bidi-display-reordering t)
(setq bidi-inhibit-bpa t
      long-line-threshold 200
      large-hscroll-threshold 200
      syntax-wholeline-max 200)

(with-eval-after-load 'so-long
  (setq so-long-threshold 5000))
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))


(provide 'init-file)
