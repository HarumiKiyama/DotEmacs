(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq right-margin-width nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; package config
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")))

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq user-full-name "Harumi Kiyama"
      user-mail-address "lucius0720@hotmail.com")

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package ivy :demand
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-count-format "%d/%d")
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package swiper)
(use-package  counsel
  :config
  (counsel-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package smartparens
  :config
  (smartparens-mode))

(use-package evil-smartparens)

(use-package evil
  :config
  (evil-mode 1)
  ;; remap emacs keymap to insert state
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package company
  :config
  (company-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))


;; add spacemacs like key binding

(use-package general
  :config
  (general-override-mode)
  (general-create-definer default-leader
                          :states '(normal)
                          :keymaps 'override
                          :prefix "SPC")
  (general-create-definer local-leader
                          :states '(normal)
                          :prefix ",")
  (default-leader
    "b" '(:ignore t :which-key "buffer")
    "f" '(:ignore t :which-key "file")
    "t" '(:ignore t :which-key "toggle")
    "fs" #'save-buffer
    "tp" #'smartparens-mode
    "bd" #'kill-this-buffer)
  ;; One escape to escape them all.
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)
  )

(use-package hydra)


;; homepage setting


(defun create-homepage-buffer ()
  (org-agenda-list))

(setq initial-buffer-choice
      #'create-homepage-buffer)


;; orgmode setting

(use-package org-plus-contrib)
