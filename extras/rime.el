;; posframe for in position display
(use-package posframe
:ensure t
)

(use-package rime
  :ensure t
  :custom
  ;; (rime-librime-root "/opt/homebrew")
  ;; (rime-librime-root "~/.emacs.d/librime/dist") 
  (rime-user-data-dir "~/.config/rime")

  (rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "Sarasa Term SC Nerd-14"
            :internal-border-width 10))

  (default-input-method "rime")
  (rime-show-candidate 'posframe)
)
