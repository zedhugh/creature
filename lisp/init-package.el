;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'info)

(defconst creature/pkg-dir
  (expand-file-name
   "../site-lisp"
   (file-name-directory(or load-file-name buffer-file-name)))
  "Package directory.")

(defconst creature/pkgs
  '((magit . ( :load "magit/lisp" :autoload "magit-autoloads" :info "magit/docs"
               :deps (compat cond-let llama transient with-editor)))
    (compat . (:load "compat" :info "compat"))
    (cond-let . (:load "cond-let" :autoload "cond-let-autoloads"))
    (llama . ( :load "llama" :autoload "llama-autoloads"
               :deps (compat)))
    (transient . ( :load "transient/lisp" :autoload "transient-autoloads"
                   :info "transient/docs" :deps (compat cond-let)))
    (with-editor . ( :load "with-editor/lisp" :autoload "with-editor-autoloads"
                     :info "with-editor/docs" :deps (compat cond-let)))
    (git-modes . ( :load "git-modes" :autoload "git-modes-autoloads"
                   :deps (compat)))
    (pinentry . (:load "pinentry" :autoload "pinentry"))
    (lazy-load . (:load "lazy-load" :autoload "lazy-load"))
    (awesome-pair . (:load "awesome-pair"))
    (editorconfig . (:load "editorconfig-emacs"))
    (emmet-mode . (:load "emmet-mode"))
    (eslint-disable-rule . (:load "eslint-disable-rule"))
    (eslint . (:load "eslint/lisp"))
    (gptel . (:load "gptel" :deps (compat transient)))
    (expand-region . (:load "expand-region"))
    (rg . (:load "rg" :info "rg" :deps (transient wgrep)))
    (wgrep . (:load "Emacs-wgrep"))
    (avy . (:load "avy"))
    (mwim . (:load "mwim"))
    (symbol-overlay . (:load "symbol-overlay"))
    (markdown-mode . (:load "markdown-mode" :deps (edit-indirect)))
    (edit-indirect . (:load "edit-indirect"))
    (emms . (:load "emms" :autoload "emms-auto" :info "emms/doc"))
    (nov . (:load "nov" :deps (esxml)))
    (esxml . (:load "esxml"))
    (org-pomodoro . (:load "org-pomodoro" :deps (alert)))
    (alert . (:load "alert" :deps (gntp)))
    (gntp . (:load "gntp"))
    (ox-hugo . (:load "ox-hugo" :autoload "ox-hugo-autoloads" :deps (tomelr)))
    (tomelr . (:load "tomelr"))
    (plantuml-mode . (:load "plantuml-mode" :deps (dash deflate)))
    (dash . (:load "dash"))
    (deflate . (:load "deflate" :deps (dash)))
    (prettier . (:load "prettier" :info "prettier" :deps (iter2 nvm)))
    (iter2 . (:load "iter2"))
    (nvm . (:load "nvm" :deps (s dash f)))
    (s . (:load "s"))
    (f . (:load "f" :deps (s dash)))
    (prisma-ts-mode . (:load "prisma-ts-mode"))
    (rime . (:load "emacs-rime" :deps (dash posframe)))
    (posframe . (:load "posframe"))
    (sdcv . (:load "sdcv" :deps (posframe)))
    (which-key . (:load "emacs-which-key"))
    (yasnippet . (:load "yasnippet"))
    (yasnippet-snippets . (:load "yasnippet-snippets" :deps (yasnippet)))
    (auto-yasnippet . (:load "auto-yasnippet" :deps (yasnippet)))
    (vimrc-mode . (:load "vimrc-mode"))
    (yaml-mode . (:load "yaml-mode"))
    (lua-mode . (:load "lua-mode"))
    (nginx-mode . (:load "nginx-mode"))
    (pdf-tools . ( :load "pdf-tools/lisp" :autoload "pdf-tools-autoloads"
                   :deps (tablist)))
    (tablist . (:load "tablist"))
    (saveplace-pdf-view . (:load "saveplace-pdf-view"))
    (graphviz-dot-mode . (:load "graphviz-dot-mode"))
    (meson-mode . (:load "meson-mode"))
    (crontab-mode . (:load "crontab-mode"))
    (marginalia . (:load "marginalia" :deps (compat)))
    (vertico . (:load ("vertico" "vertico/extensions") :deps (compat)))
    (corfu . (:load ("corfu" "corfu/extensions") :deps (compat)))
    (orderless . (:load "orderless" :deps (compat)))
    (cape . (:load "cape" :deps (compat)))
    (consult . (:load "consult" :deps (compat)))
    (corfu-terminal . (:load "emacs-corfu-terminal" :deps (corfu popon)))
    (popon . (:load "emacs-popon"))
    (embark . (:load "embark"))
    )
  "Package name and config pair.")

(defvar creature/pkgs-actived nil
  "Actived packages.
This is temporary variable for function `creature/pkg-active'.")

(cl-defun creature/pkg-active (pkg)
  "Setup package."
  (when (member pkg creature/pkgs-actived)
    (cl-return-from creature/pkg-active creature/pkgs-actived))

  (let* ((pkg-config (alist-get pkg creature/pkgs))
         (pkg-load-path (plist-get pkg-config :load))
         (pkg-autoload (plist-get pkg-config :autoload))
         (pkg-info-dir (plist-get pkg-config :info))
         (pkg-deps (plist-get pkg-config :deps)))
    (unless pkg-config (error "package(%s) not in `creature/pkgs'." pkg))

    (dolist (dep pkg-deps) (creature/pkg-active dep))
    (if (stringp pkg-load-path)
        (add-to-list 'load-path (file-name-concat creature/pkg-dir pkg-load-path))
      (dolist (path pkg-load-path)
        (add-to-list 'load-path (file-name-concat creature/pkg-dir path))))
    (when pkg-autoload (load pkg-autoload t t))
    (when pkg-info-dir
      (add-to-list 'Info-additional-directory-list
                   (file-name-concat creature/pkg-dir pkg-info-dir))))

  (add-to-list 'creature/pkgs-actived pkg))

(add-to-list 'Info-additional-directory-list
             (file-truename (file-name-concat creature/pkg-dir "../.cache")))

(add-to-list 'load-path creature/pkg-dir)


(provide 'init-package)
