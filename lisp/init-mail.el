;; -*- coding: utf-8; lexical-binding: t; -*-

(setq user-full-name "Zedhugh Chen"
      user-mail-address "605691129@qq.com")
(setq gnus-select-method
      '(nnimap "qq mail"
               (nnimap-address "imap.qq.com") ; it could also be imap.googlemail.com if that's your server.
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)
               ;; (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")  ; Move expired messages to Gmail's trash.
               (nnmail-expiry-wait immediate) ; Mails marked as expired can be processed immediately.
               ))

(setq smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq send-mail-function 'smtpmail-send-it         ;设置邮件发送方法
      message-send-mail-function 'smtpmail-send-it ;设置消息发送方法
      mail-specify-envelope-from t      ;发送邮件时指定信封来源
      mail-envelope-from 'header)       ;信封来源于 header

(setq gnus-expert-user t)
(setq gnus-asynchronous t)
(setq message-confirm-send t)
(setq message-kill-buffer-on-exit t)


(provide 'init-mail)
