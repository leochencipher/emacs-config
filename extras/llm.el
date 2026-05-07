;; gptel  -*- lexical-binding: t; -*- 
(use-package gptel
  :ensure t
  :bind ("C-c e" . gptel)
  :config
  (setq-default gptel-backend 
		(gptel-make-openai "LM Studio"
		  :host "localhost:1234"
		  :protocol "http"
		  :key "KEY_FOR_ACCESSING_OPENWEBUI"
		  :endpoint "/v1/chat/completions"
		  :stream t
		  :models '("huihui-qwen3.5-4b-claude-4.6-opus-abliterated"))
		)
  (setq-default gptel-default-mode 'org-mode)
)


(use-package minuet
  :ensure t
    :config
    (setq minuet-provider 'openai-fim-compatible)
    (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
    ;; I recommend beginning with a small context window size and incrementally
    ;; expanding it, depending on your local computing power. A context window
    ;; of 512, serves as an good starting point to estimate your computing
    ;; power. Once you have a reliable estimate of your local computing power,
    ;; you should adjust the context window to a larger value.
    (setq minuet-context-window 512)
    (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:8012/v1/completions")
    ;; an arbitrary non-null environment variable as placeholder
    ;; For Windows users, TERM may not be present in environment variables.
    ;; Consider using APPDATA instead.
    (plist-put minuet-openai-fim-compatible-options :name "Llama.cpp")
    (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
    ;; The model is set by the llama-cpp server and cannot be altered
    ;; post-launch.
    (plist-put minuet-openai-fim-compatible-options :model "PLACEHOLDER")

    ;; Llama.cpp does not support the `suffix` option in FIM completion.
    ;; Therefore, we must disable it and manually populate the special
    ;; tokens required for FIM completion.
    (minuet-set-nested-plist minuet-openai-fim-compatible-options nil :template :suffix)
    (minuet-set-optional-options
     minuet-openai-fim-compatible-options
     :prompt
     (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
         (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
                 (plist-get ctx :language-and-tab)
                 (plist-get ctx :before-cursor)
                 (plist-get ctx :after-cursor)))
     :template)

    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56))

(use-package agent-shell
    :ensure t
    :bind ("C-c u a" . agent-shell)
    ;; Add agent installation configs here
    :ensure-system-package
    ((claude . "brew install claude-code")
     (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp")))

