(eval-when-compile
  (package-initialize)
  (require 'use-package))
(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)
(setq confirm-kill-emacs nil
      menu-bar-mode nil
      tool-bar-mode nil
      )

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 180)
  )

(setq user-full-name "Harumi Kiyama"
      user-mail-address "h.kiyama0720@gmail.com")

(use-package evil
  :config
  (defadvice evil-insert-state (around emacs-state-instead-of-insert-state activate)
    (evil-emacs-state))
  (evil-mode 1))
