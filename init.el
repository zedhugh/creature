;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar creature-gc-cons-threshold
  (if (display-graphic-p) (* 64 1024 1024) (* 16 1024 1024))
  "The default value to use for `gc-cons-threshold'.
If freezing sometimes, decrease it. If stuttering, increase it.")

(defvar creature-gc-cons-upper-limit
  (if (display-graphic-p) (* 512 1024 1024) (* 128 1024 1024))
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar creature-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbage collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist
  "Temporary `file-name-handler-alist' for restore after startup.")

;; Speed up startup
(defun creature-enlarge-gc-cons-threshold ()
  "Enlarge garbage collection threshold."
  (setq gc-cons-percentage  0.6
        gc-cons-threshold   creature-gc-cons-upper-limit))

(defun creature-normalize-gc-cons-threshold ()
  "Normalize garbage collection threshold."
  (setq gc-cons-percentage  0.1
        gc-cons-threshold   creature-gc-cons-threshold))

(creature-enlarge-gc-cons-threshold)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (creature-normalize-gc-cons-threshold)
            (setq file-name-handler-alist default-file-name-handler-alist)

            (setq read-process-output-max (* 1024 1024))

            ;; GC automatically when unfocused
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state) (garbage-collect))))
              (add-hook 'focus-out-hook #'garbage-collect))

            ;; Avoid GC while minibuffer active.
            (add-hook 'minibuffer-setup-hook
                      #'creature-enlarge-gc-cons-threshold)
            (add-hook 'minibuffer-exit-hook
                      #'creature-normalize-gc-cons-threshold)))

(defconst creature/config-dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "Root directory of Creature.")

(defconst creature/cache-dir
  (expand-file-name ".cache" creature/config-dir)
  "Cache directory of Creature.")

(defconst creature/pkg-dir
  (expand-file-name "site-lisp" creature/config-dir)
  "Package directory.")

(require 'cl-lib)

(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path creature/pkg-dir)
(add-to-list 'load-path (expand-file-name "lisp" creature/config-dir) t)

(require 'lazy-load)
(lazy-load-global-keys
 '(("C-x g" . magit-status))
 "magit")

(require 'init-built-in)

(require 'which-key)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)
(which-key-mode 'toggle)

(setq custom-file (expand-file-name "custom.el" creature/cache-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (emacs-init-time))))
