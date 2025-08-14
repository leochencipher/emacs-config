(use-package ellama
  :ensure t
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
		    (make-llm-ollama
		     ;; this model should be pulled to use it
		     ;; value should be the same as you print in terminal during pull
		     :chat-model "gemma3:latest"
		     :embedding-model "gemma3:latest"))
)
