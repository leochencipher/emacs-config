;; gptel  -*- lexical-binding: t; -*- 
(use-package gptel
  :ensure t
  :bind ("C-c e" . gptel)
  :config
  (setq-default gptel-backend 
		(gptel-make-openai "omlx"
		  :host "localhost:8000"
		  :protocol "http"
		  :key "chenshuo"
		  :endpoint "/v1/chat/completions"
		  :stream t
		  :models '("gemma-4-E2B-Heretic-Uncensored-mlx-4bit"))
		)
  (setq-default gptel-default-mode 'org-mode)
)

(use-package agent-shell
    :ensure t
    :bind ("C-c u a" . agent-shell)
    ;; Add agent installation configs here
    :config
    (setq agent-shell-prefer-viewport-interaction t)
    :ensure-system-package
    ((claude . "brew install claude-code")
     (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp")))

