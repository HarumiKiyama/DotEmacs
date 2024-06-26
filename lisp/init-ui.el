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
  (if (display-graphic-p)
      (load-theme 'doom-material-dark)
    (load-theme 'doom-solarized-light)))

(use-package valign
  :hook ((markdown-mode org-mode) . valign-mode))

(use-package mood-line
  :hook (after-init . mood-line-mode))

(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(provide 'init-ui)
