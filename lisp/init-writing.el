;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))
  :config
  (when sys/macp
    (setq
     rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.2/include"
     rime-librime-root "~/.emacs.d/librime/dist"
     rime-user-data-dir "~/Library/Rime"))
  )

(use-package ispell
  :ensure nil
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))

(use-package wraplish
  ;; use manateelazycat/wraplish
  :load-path "elpa/wraplish"
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook #'(lambda () (wraplish-mode 1)))))

  
(provide 'init-writing)
