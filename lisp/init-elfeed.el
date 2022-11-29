(use-package elfeed
  :defer t)

(use-package elfeed-org
  :defer t
  :config
  (setq rmh-elfeed-org-files (list "~/org-mode/elfeed.org"))
  (elfeed-org))

(provide 'init-elfeed)
