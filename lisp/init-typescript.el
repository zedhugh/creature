;; -*- coding: utf-8; lexical-binding: t; -*-

(when creature/treesit-available
  (let ((ts-enable (treesit-ready-p 'typescript))
        (tsx-enable (treesit-ready-p 'tsx)))
    (if (and ts-enable tsx-enable)
        (progn
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

      (add-subdirs-to-load-path (expand-file-name "optional-pkg" creature/config-dir))
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
        (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))))))


(defun creature/treesit-convert-to-template ()
  "Conver the string at point to a template string by treesit."
  (interactive)
  (when (and (treesit-available-p) (treesit-language-at (point)))
    (let* ((node (treesit-node-at (point)))
           (node-type (treesit-node-type node))
           (start nil)
           (end nil)
           (pos nil))
      (when (string= node-type "string_fragment")
        (setq start (treesit-node-start node)
              end (treesit-node-end node)
              pos (point))
        (save-restriction
          (save-excursion
            (goto-char start)
            (delete-char -1)
            (insert "`")
            (goto-char end)
            (delete-char 1)
            (insert "`")))
        (goto-char pos)))))

(with-eval-after-load 'js
  (define-key js-ts-mode-map (kbd "C-c '") #'creature/treesit-convert-to-template))
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-base-mode-map (kbd "C-c '") #'creature/treesit-convert-to-template))


(provide 'init-typescript)
