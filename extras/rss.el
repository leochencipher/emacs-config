;; Configure Elfeed  -*- lexical-binding: t; -*-
(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  (elfeed-search-filter "@1-week-ago +unread")
  (browse-url-browser-function 'eww-browse-url)
  :config 
  (defun yt-dl-v-it ()
    "Downloads the URL in an async shell"
    (interactive) 
    (let ((default-directory "~/newpipe"))
      (start-process "DownloadVideo" nil "edv" (format "%s" (elfeed-entry-link elfeed-show-entry)))
      ))

  (defun yt-dl-a-it ()
    "Downloads the URL in an async shell"
    (interactive) 
    (let ((default-directory "~/newpipe"))
      (start-process "DownloadAudio" nil "eda" (format "%s" (elfeed-entry-link elfeed-show-entry)))
      ))

  (add-hook 'elfeed-show-mode-hook 'visual-line-mode)

  (run-at-time nil (* 2 60 60) #'elfeed-update)
  :bind
  (("C-c u e" . elfeed)
   :map elfeed-show-mode-map
   ("a" . yt-dl-a-it)
   ("v" . yt-dl-v-it)))

;; Configure Elfeed with org mode
(use-package elfeed-org
    :ensure t
    :config
    (elfeed-org)
    :custom
    (rmh-elfeed-org-files (list "~/org/elfeed.org")))

;; elfeed goodies
(use-package elfeed-goodies :ensure t :config (setq elfeed-goodies/entry-pane-position 'bottom) (elfeed-goodies/setup))


;; Elfeed tube for youtube content
(use-package elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

