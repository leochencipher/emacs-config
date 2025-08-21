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
                 :models (get-ollama-models))
  (setq
   gptel-model 'gemma3:latest
   gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models (get-ollama-models))))
