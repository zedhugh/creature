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
    (call-process "xdg-open" nil 0 nil file-path)))

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
;;                                    mpv                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun creature/file-mime-type (file &optional deref-symlinks)
  "Return FILE's MIME type as reported by the external `file' program.
When DEREF-SYMLINKS is non-nil and FILE is a symbolic link, inspect the
link target instead of the link itself.

Signal a `user-error' if FILE does not exist, is unreadable, or its MIME
type cannot be determined."
  (unless (executable-find "file")
    (user-error "MIME detection requires the `file' program"))
  (unless (file-exists-p file)
    (user-error "File does not exist: %s" file))
  (unless (file-readable-p file)
    (user-error "File is not readable: %s" file))
  (let ((args (append (when deref-symlinks
                        '("-L"))
                      '("--brief" "--mime-type" "--")
                      (list file)))
        (process-file-side-effects nil))
    (with-temp-buffer
      (let* ((status (apply #'process-file "file" nil t nil args))
             (mime-type (replace-regexp-in-string "\n\\'" "" (buffer-string))))
        (unless (eq status 0)
          (user-error "Failed to detect MIME type for %s: %s"
                      file
                      (if (string-empty-p mime-type)
                          (format "`file' exited with status %s" status)
                        mime-type)))
        (when (string-empty-p mime-type)
          (user-error "Empty MIME type for %s" file))
        mime-type))))

(defun creature/7z-executable ()
  "Return the available 7-Zip executable path, or nil if not found."
  (or (executable-find "7z")
      (executable-find "7zz")))

(defun creature/image-mime-type-p (mime-type)
  "Return t when MIME-TYPE names an optical disc image format.
This accepts the MIME types returned by `file' for ISO 9660 and UDF
images, plus the corresponding shared-mime-info aliases."
  (not (null (member mime-type '("application/x-udf-image"
                                 "application/x-iso9660-image"
                                 "application/vnd.efi.iso"
                                 "application/x-cd-image")))))

(defun creature/image-files (image)
  "Return a list of file paths stored in IMAGE using `7z l -slt'.
Directory entries are excluded from the returned list."
  (let ((7z-exe (creature/7z-executable)))
    (unless 7z-exe
      (user-error "Listing image contents requires `7z' or `7zz'"))
    (unless (file-exists-p image)
      (user-error "Image does not exist: %s" image))
    (unless (file-readable-p image)
      (user-error "Image is not readable: %s" image))
    (let ((process-file-side-effects nil))
      (with-temp-buffer
        (let ((status (process-file 7z-exe nil t nil "l" "-slt" "--" image))
              (current-path nil)
              files)
          (unless (eq status 0)
            (user-error "Failed to list image contents for %s" image))
          (goto-char (point-min))
          (while (not (eobp))
            (cond
             ((looking-at "^Path = \\(.*\\)$")
              (setq current-path (match-string 1)))
             ((looking-at "^Folder = -$")
              (when current-path
                (push current-path files))))
            (forward-line 1))
          (nreverse files))))))

(defun creature/bluray-iso-p (image)
  "Return non-nil when IMAGE looks like a Blu-ray ISO."
  (let (has-index has-movie-object has-playlist has-stream)
    (dolist (path (creature/image-files image))
      (cond
       ((string= path "BDMV/index.bdmv")
        (setq has-index t))
       ((string= path "BDMV/MovieObject.bdmv")
        (setq has-movie-object t))
       ((and (string-prefix-p "BDMV/PLAYLIST/" path)
             (string-suffix-p ".mpls" path))
        (setq has-playlist t))
       ((and (string-prefix-p "BDMV/STREAM/" path)
             (string-suffix-p ".m2ts" path))
        (setq has-stream t))))
    (and has-index has-movie-object has-playlist has-stream)))

(defun creature/directory-file-match-p (dir regexp)
  "Return non-nil when DIR contains a regular file matching REGEXP."
  (when (file-directory-p dir)
    (let* ((case-fold-search t)
           (path (car (directory-files dir t regexp t 1))))
      (and path (file-regular-p path)))))

(defun creature/bluray-directory-p (dir)
  "Return non-nil when DIR looks like a Blu-ray directory tree."
  (let* ((bdmv-dir (file-name-concat dir "BDMV"))
         (index-file (file-name-concat bdmv-dir "index.bdmv"))
         (movie-object-file (file-name-concat bdmv-dir "MovieObject.bdmv"))
         (playlist-dir (file-name-concat bdmv-dir "PLAYLIST"))
         (stream-dir (file-name-concat bdmv-dir "STREAM")))
    (and (file-directory-p dir)
         (file-regular-p index-file)
         (file-regular-p movie-object-file)
         (creature/directory-file-match-p playlist-dir "\\.mpls\\'")
         (creature/directory-file-match-p stream-dir "\\.m2ts\\'"))))

(defun creature/play-with-mpv (filename)
  "Play local media FILENAME with mpv, including Blu-ray directories and images."
  (cond
   ((not (executable-find "mpv"))
    (user-error "mpv: executable not found"))
   ((tramp-tramp-file-p filename)
    (user-error "mpv: cannot play remote file: %s" filename))
   ((creature/bluray-directory-p filename)
    (call-process "mpv" nil 0 nil filename))
   (t
    (let ((mime-type (creature/file-mime-type filename t)))
      (cond
       ((string-prefix-p "video/" mime-type t)
        (call-process "mpv" nil 0 nil filename))
       ((string-prefix-p "audio/" mime-type t)
        (call-process "mpv" nil 0 nil "--no-video" filename))
       ((creature/image-mime-type-p mime-type)
        (if (or (not (creature/7z-executable))
                (creature/bluray-iso-p filename))
            (call-process "mpv" nil 0 nil
                          "bd://"
                          (concat "--bluray-device=" filename))
          (user-error "mpv: unsupported image %s, MIME: %s"
                      filename mime-type)))
       (t
        (user-error "mpv: unsupported file %s, MIME: %s" filename mime-type)))))))

(defun creature/mpv ()
  (interactive)
  (let ((filename (if (derived-mode-p 'dired-mode)
                      (dired-get-file-for-visit)
                    (buffer-file-name))))
    (if filename
        (creature/play-with-mpv filename)
      (user-error "mpv: no file to play"))))

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
                          (require 'notifications)
                          (let ((msg (format-time-string "drink time: %Y-%m-%d %H:%M:%S")))
                            (notifications-notify :title msg :timeout 0 :app-icon nil))
                          ))))

(defun creature/kill-this-buffer ()
  "Kill the current buffer quietly."
  (interactive)
  (kill-buffer (current-buffer)))


(provide 'init-func)
