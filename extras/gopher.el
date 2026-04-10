;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  -*- lexical-binding: t; -*-
;;;
;;;   gopher and gemini
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpher
  :ensure t
  :demand t
  :init
  (add-hook 'elpher-mode-hook 'visual-line-mode)
  (add-hook 'elpher-mode-hook 'visual-fill-column-mode)
  (setq-default visual-fill-column-center-text t)
  :bind (("C-c u g" . elpher)
         )
  :config
  (setq visual-fill-column-center-text t)
  )

