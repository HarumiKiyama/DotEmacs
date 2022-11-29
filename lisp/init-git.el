;;; init-git.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package magit
  :commands (magit-status)
  :init
  (setq magit-refs-show-commit-count 'all
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(provide 'init-git)
