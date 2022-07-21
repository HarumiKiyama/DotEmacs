;;; init-snippets.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config)

(provide 'init-snippets)
