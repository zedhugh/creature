;; -*- coding: utf-8; lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  open file                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun creature/open-init-file ()
  "Open init file."
  (interactive)
  (find-file (expand-file-name "init.el" creature/config-dir)))

(defun creature/open-in-external-app (file-path)
  (if (eq system-type 'windows-nt)
      (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path))
    (start-process "" nil "xdg-open" file-path)))

(defun creature/open-file-or-directory-in-external-app (arg)
  (interactive "P")
  (if arg
      (creature/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (creature/open-in-external-app file-path)
        (message "No file associated to this buffer")))))

(defun creature/sudo-edit (&optional arg)
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               indentation                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar creature/indent-sensitive-modes
  '(asm-mode
    coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes which disable auto-indenting.")

(defun creature/fallback-indent-func ()
  (unless (member major-mode creature/indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-region (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun creature/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (if (and (bound-and-true-p prettier-mode)
           (bound-and-true-p prettier-version)
           (not (region-active-p)))

      (progn
        (save-excursion
          (prettier-prettify))
        (message "Indented prettier buffer."))

    (creature/fallback-indent-func)))

(defvar creature/drink-timer nil
  "Timer for drink water notifition.")
(defun creature/start-drink-timer ()
  "Start timer for notice drink water."
  (interactive)
  (when (timerp creature/drink-timer)
    (cancel-timer creature/drink-timer)
    (setq creature/drink-timer nil))
  (setq creature/drink-timer
        (run-with-timer 1200 1200
                        (lambda ()
                          (require 'alert)
                          (alert-notifications-notify '(:title "该喝水啦！！！" :persistent 1))))))


(provide 'init-func)
