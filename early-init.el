(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

;; Initialize frame with alpha
(add-to-list 'default-frame-alist '(alpha-background . 85))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
