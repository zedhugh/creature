;; -*- coding: utf-8; lexical-binding: t; -*-

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


(add-to-list 'auto-mode-alist '("package\\.env\\'"             . conf-mode))
(add-to-list 'auto-mode-alist '("package\\.use\\'"             . conf-mode))
(add-to-list 'auto-mode-alist '("package\\.mask\\'"            . conf-mode))
(add-to-list 'auto-mode-alist '("package\\.license\\'"         . conf-mode))
(add-to-list 'auto-mode-alist '("package\\.keywords\\'"        . conf-mode))
(add-to-list 'auto-mode-alist '("package\\.accept_keywords\\'" . conf-mode))


(provide 'init-file)
