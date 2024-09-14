;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-package)
(add-pkg-in-pkg-dir "emacs-websocket")

(require 'websocket)

(setq-local websocket-eldoc-callback nil)
(defun test-websocket-eldoc (callback)
  (setq websocket-eldoc-callback
        (if (compiled-function-p callback) callback nil)))
(set (make-local-variable 'eldoc-documentation-strategy)
     'eldoc-documentation-compose-eagerly)

(run-with-timer 1 nil
                (lambda ()
                  (funcall websocket-eldoc-callback "hel")))

(test-websocket-eldoc "hel")

(make-local-variable 'eldoc-documentation-functions)
(add-to-list 'eldoc-documentation-functions #'test-websocket-eldoc)

(setq ws-server (websocket-server
                 4000
                 :host 'local
                 :on-message (lambda (ws frame)
                               (when (and (boundp 'websocket-eldoc-callback) (compiled-function-p websocket-eldoc-callback))
                                 (websocket-send-text ws (format "eldoc showed: %s" (websocket-frame-text frame)))
                                 (funcall websocket-eldoc-callback (websocket-frame-text frame))))))

(funcall (symbol-value (obarray-get obarray "websocket-eldoc-callback")) "hello")

(symbol-value 'websocket-eldoc-callback)

(websocket-server-close ws-server)

(funcall websocket-eldoc-callback "hello")
