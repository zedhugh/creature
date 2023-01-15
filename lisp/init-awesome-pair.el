;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)


(with-eval-after-load 'awesome-pair
  ;; (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  ;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  ;; (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  ;; (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  ;; (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)
  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)
  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'creature/awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-N") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-P") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-RET") 'awesome-pair-jump-out-pair-and-newline))

(defun creature/awesome-pair-in-template-string-p ()
  (and (awesome-pair-in-string-p)
       (consp (awesome-pair-string-start+end-points))

       (let* ((pos_cons (awesome-pair-string-start+end-points))
              (start (car pos_cons))
              (end (cdr pos_cons)))
         (and (char-equal ?` (char-after start))
              (char-equal ?` (char-after end))))))

(defun creature/awesome-pair-kill-template-string-curly-content ()
  (interactive)
  (save-excursion
    (kill-region (point) (save-excursion (search-forward "}") (1- (point))))))

(defun creature/in-elisp-string-symbol ()
  (when (and (derived-mode-p 'emacs-lisp-mode))
    (let ((faces (get-text-property (point) 'face)))
      (and (listp faces)
           (memq 'font-lock-constant-face faces)
           (or (memq 'font-lock-doc-face faces)
               (memq 'font-lock-string-face faces)
               (memq 'font-lock-comment-face faces))))))

(defun creature/awesome-pair-kill ()
  (interactive)
  (cond ((and (derived-mode-p 'js-mode
                              'typescript-mode
                              'typescript-ts-base-mode
                              'web-mode)
              (creature/awesome-pair-in-template-string-p)
              (awesome-pair-in-curly-p))
         (creature/awesome-pair-kill-template-string-curly-content))
        ((creature/in-elisp-string-symbol)
         (kill-region (point)
                      (save-excursion
                        (search-forward "'" (pos-eol))
                        (1- (point)))))
        (t
         (funcall-interactively #'awesome-pair-kill))))

(defun creature/awesome-pair-setup ()
  (require 'awesome-pair)
  (add-hook 'prog-mode-hook #'awesome-pair-mode)
  (add-hook 'conf-unix-mode-hook #'awesome-pair-mode)
  (add-hook 'conf-windows-mode-hook #'awesome-pair-mode)
  (add-hook 'yaml-mode-hook #'awesome-pair-mode)
  nil)

(creature/awesome-pair-setup)


(provide 'init-awesome-pair)
