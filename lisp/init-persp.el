;;; init-persp.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c p")) ; pick your own prefix key here
  :init
  (setq persp-state-default-file (expand-file-name ".cache/harumi" user-emacs-directory))
  (setq persp-show-modestring 'header)
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save))


(provide 'init-persp)
