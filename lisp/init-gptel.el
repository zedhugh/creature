;; -*- coding: utf-8; lexical-binding: t; -*-

(add-pkg-in-pkg-dir "gptel")

(let ((gptel-autoload-file
       (file-name-concat creature/cache-dir "gptel-autoloads.el")))
  (unless (file-exists-p gptel-autoload-file)
    (loaddefs-generate (file-name-concat creature/pkg-dir "gptel")
                       gptel-autoload-file))
  (load-file gptel-autoload-file))

(with-eval-after-load 'gptel
  (let (deepseek bigmodel bigmodel-code)
    (setq deepseek
          (gptel-make-openai "DeepSeek"
            :host "api.deepseek.com"
            :endpoint "/chat/completions"
            :stream t
            :key (lambda () (gptel-api-key-from-auth-source "api.deepseek.com"))
            :models '(deepseek-v4-flash deepseek-v4-pro))
          bigmodel
          (gptel-make-openai "BigModel"
            :host "open.bigmodel.cn"
            :endpoint "/api/paas/v4/chat/completions"
            :stream t
            :key (lambda () (gptel-api-key-from-auth-source "open.bigmodel.cn"))
            :models '(glm-5.1))
          bigmodel-code
          (gptel-make-openai "BigModel-Coding"
            :host "open.bigmodel.cn"
            :endpoint "/api/coding/paas/v4/chat/completions"
            :stream t
            :key (lambda () (gptel-api-key-from-auth-source "open.bigmodel.cn"))
            :models '(glm-5.1)))

    (setq gptel-default-mode #'org-mode
          gptel-backend bigmodel-code
          gptel-model 'gml-5.1
          )))

(provide 'init-gptel)
