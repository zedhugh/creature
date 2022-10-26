;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'typescript-mode)


(define-derived-mode typescript-tsx-mode typescript-mode "Tsx"
  "Major mode for editing TSX file."
  (require 'tree-sitter)
  (require 'tree-sitter-hl)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query)

  (require 'tsi-typescript)
  (tsi-typescript-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'"  . typescript-tsx-mode))

(with-eval-after-load 'tree-sitter-langs
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))


(provide 'init-typescript)
