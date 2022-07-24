;;; init-git.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package magit
  :commands (magit-status)
  :init
  (setq magit-refs-show-commit-count 'all))

(provide 'init-git)
