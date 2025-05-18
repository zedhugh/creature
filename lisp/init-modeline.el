;; -*- coding: utf-8; lexical-binding: t; -*-

;; buffer name
(defvar-local creature/mode-line-buffer-info
  (propertized-buffer-identification "%b")
  "custom version of `mode-line-buffer-identification'.")
(put 'creature/mode-line-buffer-info 'risky-local-variable t)

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
        (propertize
         (format " [%d|%d]" length line)
         'help-echo "Selected region info"))))
  "Length of marked string.")
(put 'creature/mode-line-region-info 'risky-local-variable t)

(defvar creature/mode-line-region-info
  '(:eval
    (when (and (region-active-p) (eq creature/focus-window (selected-window)))
      (let ((length (- (region-end) (region-beginning)))
            (line (- (line-number-at-pos (region-end))
                     (line-number-at-pos (region-beginning))
                     -1)))
        (propertize
         (format " [%d|%d]" length line)
         ))))
  "Length of marked string.")
(put 'creature/mode-line-region-info 'risky-local-variable t)

(defvar creature/mode-line-line-and-column
  `(line-number-mode
    (column-number-mode
     (column-number-indicator-zero-based
      (:propertize
       mode-line-position-column-line-format
       display (min-width (6.0))
       ,@mode-line-position--column-line-properties)
      (:propertize
       (:eval (string-replace
               "%c" "%C" (car mode-line-position-column-line-format)))
       display (min-width (6.0))
       ,@mode-line-position--column-line-properties))
     (:propertize
      mode-line-position-line-format
      display (min-width (3.0))
      ,@mode-line-position--column-line-properties))
    (column-number-mode
     (column-number-indicator-zero-based
      (:propertize
       mode-line-position-column-format
       display (min-width (3.0))
       ,@mode-line-position--column-line-properties)
      (:propertize
       (:eval (string-replace
               "%c" "%C" (car mode-line-position-column-format)))
       display (min-width (6.0))
       ,@mode-line-position--column-line-properties))))
  "Line number and column number of position.")
(put 'creature/mode-line-line-and-column 'risky-local-variable t)

(defvar creature/mode-line-percent-size
  '(:eval
    (let* ((show-brace (and mode-line-percent-position size-indication-mode))
           (begin-brace (if show-brace " {" " "))
           (end-brace (if show-brace "}" ""))
           (comma (if show-brace "," "")))
      `(,begin-brace
        (:propertize
         ("" mode-line-percent-position)
         local-map ,mode-line-column-line-number-mode-map
         display (min-width (3.0))
         mouse-face mode-line-highlight
         ;; XXX needs better description
         help-echo "Window Scroll Percentage
mouse-1: Display Line and Column Mode Menu")
        ,comma
        (size-indication-mode
         ,(propertize
           "%I"
           'local-map mode-line-column-line-number-mode-map
           'mouse-face 'mode-line-highlight
           ;; XXX needs better description
           'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu"))
        ,end-brace)))
  "Percent offset and buffer size.")
(put 'creature/mode-line-percent-size 'risky-local-variable t)

(setq mode-line-position
      `(,creature/mode-line-percent-size
        ,creature/mode-line-line-and-column))

(let ((mode-line
       '("%e"
         mode-line-front-space
         (:propertize
          ("" mode-line-mule-info mode-line-client mode-line-modified
           mode-line-remote mode-line-window-dedicated))
         mode-line-frame-identification
         creature/mode-line-buffer-info
         mode-line-position
         creature/mode-line-region-info
         " "

         mode-line-modes

         (project-mode-line project-mode-line-format)
         (vc-mode vc-mode)

         (flymake-mode flymake-mode-line-format)
         mode-line-misc-info
         mode-line-end-spaces)))
  (setq-default mode-line-format mode-line))

;; cursor position info
(size-indication-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; don't show minor modes exclude company-mode
(defun creature/mode-line-change-modes ()
  (if (boundp 'mode-line-collapse-minor-modes)
      (progn
        (setq mode-line-collapse-minor-modes
              '(not company-mode global-company-mode))
        (setq mode-line-collapse-minor-modes-to ""))
    (if (boundp 'mode-line-minor-modes)
        (setq mode-line-minor-modes nil)
      (setq minor-mode-alist nil))))
(add-hook 'emacs-startup-hook #'creature/mode-line-change-modes)

(setq display-time-format " %H:%M")
(setq display-time-load-average nil)
(setq display-time-default-load-average nil)
(display-time-mode)


(provide 'init-modeline)
