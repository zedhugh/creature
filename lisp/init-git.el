;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "magit")
(add-pkg-in-pkg-dir "dash")
(add-pkg-in-pkg-dir "git-modes")
(add-pkg-in-pkg-dir "with-editor")

(require 'lazy-load)

(lazy-load-global-keys
 '(("C-x g" . magit-status))
 "magit")

(require 'init-keybindings)
(lazy-load-local-keys
 '(("gb"  . magit-blame)
   ("gc"  . magit-clone)
   ("gff" . magit-find-file)
   ("gfc" . magit-find-git-config-file)
   ("gfs" . magit-stage-file)
   ("gi"  . magit-init)
   ("gl"  . magit-list-repositories)
   ("gs"  . magit-status))
 creature/map "magit")

;; gpg config
(defun kill-gpg-agent-when-emacs-exit ()
  "Kill `gpg-agent' for security when Emacs be killed."
  (when (and (bound-and-true-p epg-gpgconf-program)
             (executable-find epg-gpgconf-program))
    (start-process "" nil epg-gpgconf-program "--kill" "gpg-agent")))
(add-hook 'kill-emacs-hook #'kill-gpg-agent-when-emacs-exit)

(setq epg-pinentry-mode 'loopback)

(require 'git-modes)
(with-eval-after-load 'magit
  ;; (require 'forge)

  ;; pinentry for prompting password of gpg when sign git commit
  (condition-case nil
      (progn
        (require 'pinentry)
        (pinentry-start))
    (error nil))

  (setq magit-revision-show-gravatars
        '("^Author:     " . "^Commit:     ")))

;;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(with-eval-after-load 'project
  (require 'magit))


(provide 'init-git)
