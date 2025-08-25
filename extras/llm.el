;; gptel 
(use-package gptel
  :ensure t
  :bind ("C-c e" . gptel)
  :config
  (defun get-ollama-models ()
  "Fetch the list of installed Ollama models."
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         models)
    (dolist (line (cdr lines))  ; Skip the first line
      (when (string-match "^\\([^[:space:]]+\\)" line)
        (push (match-string 1 line) models)))
    (nreverse models)))

  (gptel-make-ollama "rOllama"
                 :host "192.168.1.28:11434"
                 :stream t
                 :models '(gemma3:latest))
  (setq
   gptel-model 'gemma3:latest
   gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models (get-ollama-models))))

;; gptel prompt
(use-package gptel-prompts
  :after (gptel)
  :demand t
  :config
  (setq gptel-prompts-directory "~/.config/emacs/prompts/")
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers))


;; minuet for code completion
(use-package minuet
  :ensure t
  :bind
    (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("C-c u m" . #'minuet-configure-provider)
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))
  :init
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  ;; I recommend beginning with a small context window size and incrementally
  ;; expanding it, depending on your local computing power. A context window
  ;; of 512, serves as an good starting point to estimate your computing
  ;; power. Once you have a reliable estimate of your local computing power,
  ;; you should adjust the context window to a larger value.
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder.
  ;; For Windows users, TERM may not be present in environment variables.
  ;; Consider using APPDATA instead.
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")
  
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56))
