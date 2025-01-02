;; -*- coding: utf-8; lexical-binding: t; -*-

(add-pkg-in-pkg-dir "gptel")

(let ((gptel-autoload-file
       (file-name-concat creature/cache-dir "gptel-autoloads.el")))
  (unless (file-exists-p gptel-autoload-file)
    (loaddefs-generate (file-name-concat creature/pkg-dir "gptel")
                       gptel-autoload-file))
  (load-file gptel-autoload-file))

(with-eval-after-load 'gptel
  (setq gptel-model 'deepseek-coder
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (lambda ()
                 (gptel-api-key-from-auth-source "api.deepseek.com"))
          :models '(deepseek-chat deepseek-coder))))

(provide 'init-gptel)
