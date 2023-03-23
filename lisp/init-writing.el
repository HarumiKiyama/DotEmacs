;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10)))


(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell-correct)

(use-package ispell
  :ensure nil
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))


(use-package pangu-spacing
  :defer t
  :init (progn (global-pangu-spacing-mode 1)
               ;; Always insert `real' space in org-mode.
               (add-hook 'org-mode-hook
                         (lambda ()
                           (setq-local pangu-spacing-real-insert-separtor t)))))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(provide 'init-writing)
