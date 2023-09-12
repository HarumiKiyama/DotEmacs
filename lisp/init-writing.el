;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10)))

(use-package ispell
  :ensure nil
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))

(defun pangu-real-space-hook ()
  (setq-local pangu-spacing-real-insert-separtor t))

(use-package pangu-spacing
  :defer t
  ;; Always insert `real' space in org-mode.
  :hook ((org-mode . pangu-spacing-real-insert-separtor)
         (org-journal-mode . pangu-spacing-real-insert-separtor))
  :init
  (global-pangu-spacing-mode 1))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(provide 'init-writing)
