;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))
  )

(use-package ispell
  :ensure nil
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))

(use-package wraplish
  ;; use manateelazycat/wraplish
  :load-path "elpa/wraplish")

  
(provide 'init-writing)
