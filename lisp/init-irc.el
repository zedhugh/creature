;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'erc
  (add-to-list 'erc-modules 'sasl)
  (add-to-list 'erc-modules 'services)
  (erc-update-modules)

  (setq erc-server "irc.libera.chat"
        erc-sasl-user "zedhugh"
        erc-nick "zedhugh"
        erc-user-full-name "Zedhugh Chen"
        erc-email-userid "zedhugh@gmail.com"
        ;; erc-session-client-certificate t
        erc-prompt-for-password nil

        erc-prompt-for-nickserv-password nil
        erc-use-auth-source-for-nickserv-password t

        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t

        erc-autojoin-channels-alist '(("libera.chat" "#linuxba"))
        ))


(provide 'init-irc)
