;; Configure Elfeed
(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  (elfeed-search-filter "@1-week-ago +unread")
  :config 
  (defun yt-dl-v-it ()
    "Downloads the URL in an async shell"
    (interactive) 
    (let ((default-directory "~/newpipe"))
      (async-shell-command (format "edv '%s'" (elfeed-entry-link elfeed-show-entry)))))

  (defun yt-dl-a-it ()
    "Downloads the URL in an async shell"
    (interactive) 
    (let ((default-directory "~/newpipe"))
      (async-shell-command (format "eda '%s'" (elfeed-entry-link elfeed-show-entry)))))

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
