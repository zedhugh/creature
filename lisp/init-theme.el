;; -*- coding: utf-8; lexical-binding: t; -*-

;; Pulse
(defvar creature/pulse-enable t
  "Whether enable pulse for scroll and switch window.")

(defun creature/pulse-line (&rest _)
  "Pulse the current line."
  (when creature/pulse-enable
    (pulse-momentary-highlight-one-line (point))))

;; pulse setup, like beacon
(with-eval-after-load 'pulse
  (setq pulse-delay 0.04))

(dolist (command '(scroll-up
                   scroll-down
                   recenter))
  (advice-add command :after #'creature/pulse-line))
(add-to-list 'window-selection-change-functions #'creature/pulse-line)


;; address style
(add-hook 'erc-mode-hook #'goto-address-mode)
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)


(autoload 'rainbow-delimiters-mode "rainbow-delimiters")
(autoload 'rainbow-identifiers-mode "rainbow-identifiers")
(dolist (mode '(rainbow-delimiters-mode rainbow-identifiers-mode))
  (add-hook 'prog-mode-hook mode))


(global-whitespace-mode 1)
(setq whitespace-style
      '(tabs
        trailing
        lines
        space-before-tab
        newline
        indentation
        empty
        space-after-tab
        tab-mark
        missing-newline-at-eof))

(defun creature/show-trailing-whitespace ()
  (when (buffer-file-name)
    (setq show-trailing-whitespace t)))
(add-hook 'find-file-hook #'creature/show-trailing-whitespace)

(blink-cursor-mode -1)

(require 'circadian)
(when (and (fboundp 'circadian-setup)
           (custom-theme-name-valid-p 'modus-vivendi))
  (setq circadian-themes '(("08:00" . modus-operandi)
                           ("18:00" . modus-vivendi)))
  (circadian-setup))


(defconst creature/font-config
  ;; '(("Operator Mono Book" . 16))
  (if (eq system-type 'windows-nt)
      (if (> (frame-pixel-width) 2560)
          '(("等距更纱黑体 SC" . 24))
        '(("等距更纱黑体 SC" . 16)))
    '(("Sarasa Mono SC" . 16)))
  ;; '(("Source Code Pro" . 16))
  "Font config.
It's a list of single-byte and multi-byte font.
Each font conf looks like (FAMILY . SIZE).")

(defun creature/fontset (&optional frame)
  "Config fonts for FRAME.
if FRAME is nil, setup for current frame."
  ;; single-byte code
  (setq inhibit-compacting-font-caches
        (if (eq system-type 'window-nt) t nil))

  (let ((single (car creature/font-config))
        (multi  (cdr creature/font-config)))
    (let ((family   (car single))
          (size     (cdr single)))
      (when (or family size)
        (condition-case nil
            (set-face-attribute
             'default frame
             :font (font-spec :family family :size size))
          (error nil))))
    (let ((family   (car multi))
          (size     (cdr multi)))
      (when (member family (font-family-list))
        (dolist (charset '(kana han cjk-misc bopomofo))
          (condition-case nil
              (set-fontset-font
               t charset
               (font-spec :family family :size size) frame)
            (error nil)))))))

(add-hook 'emacs-startup-hook #'creature/fontset)
(add-hook 'server-after-make-frame-hook #'creature/fontset)


(provide 'init-theme)
