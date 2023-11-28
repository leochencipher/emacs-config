;; epub reader
(use-package shrface
  :ensure t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package visual-fill-column
  :ensure t
  :config
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-width 120)
)

(defun my-nov-view-setup ()
    (face-remap-add-relative 'variable-pitch :family "Amazon Ember"
                                           :height 1.1))

(use-package nov
  :ensure t
  :after
  visual-fill-column
  :init
  (add-hook 'nov-mode-hook #'shrface-mode) ;
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (add-hook 'nov-mode-hook 'my-nov-view-setup)
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("j" . scroll-up-line)
              ("k" . scroll-down-line)
	      ("t" . nov-goto-toc)
	      ("n" . nov-next-document)
	      ("p" . nov-previous-document)
	      ("<tab>" . shrface-outline-cycle)
	)
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (setq nov-text-width 110)
  )
