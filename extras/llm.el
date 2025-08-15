(use-package gptel
  :ensure t
  :bind ("C-c e" . gptel)
  :config
  (setq
   gptel-model 'gemma3:latest
   gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(gemma3:latest))))
