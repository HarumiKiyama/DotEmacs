;;; init-ui.el -*- lexical-binding: t no-byte-compile: t -*-

(setq inhibit-splash-screen t)
(setq  initial-frame-alist (quote ((fullscreen . maximized))))
(global-hl-line-mode t)
(menu-bar-mode -1)
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

(use-package doom-themes
  :ensure t
  :init
  (progn
    (load-theme 'doom-dracula)
    (set-face-attribute 'default nil :height 150 :family "dejavu")))



(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes t)
  :hook (after-init . doom-modeline-mode))

(use-package visual-fill-column
  :init
(setq visual-fill-column-width 110
      visual-fill-column-center-text t))
(provide 'init-ui)
