;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)
(require 'rime)


(setq default-input-method "rime")
(setq rime-show-candidate 'posframe)
(setq rime-posframe-style 'simple)
(setq rime-posframe-fixed-position t)
(setq rime-disable-predicates
      '(rime-predicate-prog-in-code-p
        rime-predicate-hydra-p))

(lazy-load-set-keys
 '(("M-i" . rime-force-enable))
 rime-mode-map)

(lazy-load-set-keys
 '(("M-i" . rime-inline-ascii)
   ("M-o" . rime--backspace)
   ("M-h" . rime--escape))
 rime-active-mode-map)


(provide 'init-rime)