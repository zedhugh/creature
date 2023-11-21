;; -*- coding: utf-8; lexical-binding: t; -*-

(defun creature/set-mode-line-format-for-exist-buffers ()
  "Make customized mode line works in exist buffers."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (setq mode-line-format creature/mode-line-format)))
        (buffer-list)))


;; buffer name
(defvar creature/mode-line-buffer-name
  '(:eval (propertize
           "%b"
           'face 'mode-line-buffer-id))
  "Buffer name with face.")
(set-face-attribute 'mode-line-buffer-id nil :weight 'normal)
(put 'creature/mode-line-buffer-name 'risky-local-variable t)

;; marker region info
(defvar creature/focus-window nil
  "Current focus window.")

(defun creature/save-current-window ()
  "Save current window."
  (setq creature/focus-window (selected-window)))

(add-hook 'post-command-hook
          #'creature/save-current-window)

(defvar creature/mode-line-region-info
  '(:eval
    (when (and (region-active-p) (eq creature/focus-window (selected-window)))
      (let ((length (- (region-end) (region-beginning)))
            (line (- (line-number-at-pos (region-end))
                     (line-number-at-pos (region-beginning))
                     -1)))
        (format " [%d|%d]" length line))))
  "Length of marked string.")
(put 'creature/mode-line-region-info 'risky-local-variable t)

(defvar creature/mode-line-company-info
  '(:eval
    (when (and (not buffer-read-only)
               (or (bound-and-true-p company-mode)
                   (bound-and-true-p global-company-mode)))
      company-lighter))
  "Customize company lighter.")
(put 'creature/mode-line-company-info 'risky-local-variable t)

;; combin mode line fromat
(defvar creature/mode-line-format
  '("%e"
    creature/mode-line-window-number
    " "
    current-input-method-title
    "%Z" ; coding system and eol type
    "%*" ; read only buffer?
    "%+" ; buffer modified?
    "%@" ; buffer is in remote?
    " "
    creature/mode-line-buffer-name
    " {"
    "%p" ; percent of point in buffer
    ","
    "%I" ; buffer size
    "}("
    "%l,%c" ; line and column
    ")"
    " ("
    mode-name ; major mode
    mode-line-process
    creature/mode-line-company-info
    ")"
    creature/mode-line-region-info
    (vc-mode vc-mode)

    (flymake-mode flymake-mode-line-format)
    mode-line-misc-info
    mode-line-end-spaces
    )
  "Customized mode line format.")

;; 1. define a variable to keep origin mode-line
;; 2. make customized mode-line worked for exist buffers.
(defvar creature/origin-mode-line-format
  mode-line-format
  "Keep origin `mode-line-format'")

(setq-default mode-line-format creature/mode-line-format)

(creature/set-mode-line-format-for-exist-buffers)

(defun creature/toggle-mode-line ()
  "Switch `mode-line-format' between customized and the origin.
Customized is save in `creature/mode-line-format',
orgiin is in `creature/origin-mode-line-format'."

  (let ((tmp-mode-line
         (if (eq mode-line-format creature/mode-line-format)
             creature/origin-mode-line-format
           creature/mode-line-format)))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (setq mode-line-format tmp-mode-line)))
          (buffer-list))
    (setq-default mode-line-format tmp-mode-line)
    nil))

;; (setq display-time-interval 1)
(setq display-time-format " %H:%M")
(setq display-time-load-average nil)
(setq display-time-default-load-average nil)
(display-time-mode)


(provide 'init-modeline)
