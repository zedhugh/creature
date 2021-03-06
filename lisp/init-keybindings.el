;; -*- coding: utf-8; lexical-binding: t; -*-

(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-M-\\") 'creature/indent-region-or-buffer)
(global-set-key  (kbd "C-c '") (kbd "`"))

(setq backward-delete-char-untabify-method 'hungry)
(global-set-key (kbd "DEL") 'backward-delete-char-untabify)

;;; mwim
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)

(defconst creature/prefix-key "M-m"
  "Prefix key for `creature-map'.")

(define-prefix-command 'creature-map)
(global-set-key (kbd creature/prefix-key) 'creature-map)
;; (define-key special-mode-map (kbd creature/prefix-key) 'creature-map)

(creature/set-keys creature-map
                   ;; evil-leader/set-key
                   "bb" 'switch-to-buffer
                   "bd" 'kill-current-buffer
                   "be" 'eval-buffer

                   "cl" 'evilnc-comment-or-uncomment-lines
                   "cp" 'evilnc-comment-or-uncomment-paragraphs

                   "el" 'creature/toggle-flycheck-error-list
                   "ex" 'flycheck-display-error-at-point

                   "fi" 'creature/open-init-file
                   "fe" 'creature/open-early-init-org-file
                   "fo" 'creature/open-file-or-directory-in-external-app
                   "fj" 'dired-jump
                   "ff" 'find-file
                   "fp" 'find-file-at-point
                   "fr" 'counsel-recentf
                   "fs" 'save-buffer

                   "gc"  'magit-clone
                   "gff" 'magit-find-file
                   "gfc" 'magit-find-git-config-file
                   "gfs" 'magit-stage-file
                   "gi"  'magit-init
                   "gl"  'magit-list-repositories
                   "gs"  'magit-status

                   "hf" 'describe-function
                   "hF" 'describe-face
                   "hv" 'describe-variable
                   "hk" 'describe-key
                   "ho" 'describe-symbol
                   "hp" 'describe-package
                   "qk" 'save-buffers-kill-emacs
                   "qq" 'save-buffers-kill-terminal

                   "sc" 'evil-surround-change
                   "sd" 'evil-surround-delete
                   "ss" 'evil-surround-region

                   "v" 'er/expand-region
                   "w" 'hydra-window/body

                   "ys" 'youdao-dictionary-search-at-point
                   "yp" 'youdao-dictionary-play-voice-at-point

                   ;; jump by avy
                   "jl" 'avy-goto-line
                   "jw" 'avy-goto-word-1

                   "SPC" 'execute-extended-command)

;; (when sys/win32p
;;   ;; (w32-register-hot-key [s-t])
;;   (setq-default w32-apps-modifier 'hyper)
;;   (setq-default w32-lwindow-modifier 'super))

(provide 'init-keybindings)
