;; -*- coding: utf-8; lexical-binding: t; -*-

(defun enable-socks-proxy ()
  (interactive)
  (require 'socks)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq socks-override-functions t)

  (setq url-gateway-method 'socks))

(defun disable-socks-proxy ()
  (interactive)
  (setq url-gateway-method 'native))


(provide 'init-proxy)
