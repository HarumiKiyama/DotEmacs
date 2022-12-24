;;; init-ui.el -*- lexical-binding: t no-byte-compile: t -*-

(setq inhibit-splash-screen t)
(setq-default cursor-type 'bar)

(setq  initial-frame-alist (quote ((fullscreen . maximized))))

(global-hl-line-mode t)

(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

(use-package doom-themes
  :init
  (set-face-attribute 'default nil :height 180 :family "FiraCode Nerd Font")
  (load-theme 'doom-dracula))

(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes t)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :hook (after-init . doom-modeline-mode))

(use-package visual-fill-column
  :init
(setq visual-fill-column-width 110
      visual-fill-column-center-text t))

(provide 'init-ui)
