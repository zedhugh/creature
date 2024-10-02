;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)

(add-pkg-in-pkg-dir "prisma-ts-mode")
(autoload 'prisma-ts-mode "prisma-ts-mode" "" t)
;; remember run `treesit-install-language-grammar' command to install `prisma'
(add-to-list
 'treesit-language-source-alist
 (list
  'prisma
  (file-truename
   (file-name-concat
    (file-name-directory (or load-file-name buffer-file-name))
    "../tree-sitter-langs/tree-sitter-prisma"))))
(add-to-list 'auto-mode-alist '("\\.prisma$" . prisma-ts-mode))

;; language server is `@prisma/language-server', install it by npm/pnpm/yarn.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(prisma-ts-mode . ("prisma-language-server" "--stdio"))))

(add-hook 'prisma-ts-mode-hook #'eglot-ensure)


(provide 'init-prisma)
