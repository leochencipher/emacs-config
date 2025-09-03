;; -*- lexical-binding: t; -*-
(use-package emms
	    :ensure t
	    :bind ("C-c u p" . emms)
	    :config
	    (setq emms-player-list '(emms-player-mpv))
 )
