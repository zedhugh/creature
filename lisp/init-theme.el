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


(provide 'init-theme)
