;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "org-pomodoro")
(add-pkg-in-pkg-dir "alert")
(add-pkg-in-pkg-dir "gntp")
(add-pkg-in-pkg-dir "ox-hugo")
(add-pkg-in-pkg-dir "tomelr")


;; customize mode for src lang
(defconst creature/org-src-lang-modes
  (if (bound-and-true-p creature/treesit-available)
      '(("js"   . js)
        ("ts"   . typescript-ts)
        ("tsx"  . tsx-ts)
        ("html" . mhtml)
        ("json" . js-json))
    '(("js"   . js)
      ("ts"   . typescript)
      ("tsx"  . typescript-tsx)
      ("html" . mhtml)
      ("json" . js-json)))
  "Better src lang reflex to mode.")

;; enable code block in org file
(defconst creature/org-src-enable-lang
  '((C          . t)
    (js         . t)
    (latex      . t)
    (shell      . t)
    (python     . t)
    (plantuml   . t)
    (emacs-lisp . t))
  "Enabled lang in org src code block.")

(autoload 'org-pomodoro "org-pomodoro" "" t)
(with-eval-after-load 'org-pomodoro
  (setq org-pomodoro-audio-player "mpv"))

;; 保存链接
(defun creature/org-capture-setup ()
  (require 'org-capture)
  (require 'org-protocol)
  (setq org-capture-templates
        '(("" "org-protocol" entry (file "~/org/bookmarks.org")
           "* TODO Review %a\n  %T:initial\n" :immediate-finish t)
          ))
  (setq org-protocol-default-template-key ""))

(run-with-idle-timer 10 nil #'creature/org-capture-setup)

(with-eval-after-load 'org
  ;; (setq org-adapt-indentation t)

  ;; show inline image when open org file
  (creature/org-capture-setup)
  (add-hook 'org-mode-hook 'org-display-inline-images)

  ;; org agenda
  (setq org-agenda-files '("~/org"))
  (global-set-key (kbd "C-c a") 'org-agenda)
  ;; enable scale image
  (setq org-image-actual-width nil)

  ;; config latex preview
  (setq org-preview-latex-default-process 'dvipng)
  (setq org-preview-latex-image-directory
        (expand-file-name "ltximg/" creature/cache-dir))

  ;; don't prompt before eval code
  (setq org-confirm-babel-evaluate nil)

  ;; sub-superscripts
  (setq org-export-with-sub-superscripts '{})

  ;; make options configged before work
  (org-babel-do-load-languages
   'org-babel-load-languages
   creature/org-src-enable-lang)
  (dolist (src2mode creature/org-src-lang-modes)
    (add-to-list 'org-src-lang-modes src2mode))

  ;; (define-key org-mode-map (kbd "RET") 'org-return-indent)
  )

(with-eval-after-load 'ox
  (setq org-export-preserve-breaks t)
  (require 'ox-hugo))

(with-eval-after-load 'ob-plantuml
  (setq org-plantuml-exec-mode 'plantuml))

(with-eval-after-load 'org-src
  (defvar org-src-lang-modes)
  (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot)))


(provide 'init-org)
