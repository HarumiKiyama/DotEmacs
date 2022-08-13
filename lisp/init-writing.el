;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell-correct
  :ensure t
  :init)

(use-package ispell
  :ensure nil
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))


(use-package olivetti
  :init
  (setq olivetti-body-width nil)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (olivetti-mode t)
      (progn
        (olivetti-mode 0))))
  :bind
  (("<f9>" . distraction-free)))


(use-package ox-hugo
  :ensure t                             ;Auto-install the package from Melpa
  :pin melpa ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :commands org-hugo-export-to-md)

(use-package pangu-spacing
  :defer t
  :init (progn (global-pangu-spacing-mode 1)

               ;; Always insert `real' space in org-mode.
               (add-hook 'org-mode-hook
                         (lambda ()
                           (setq-local pangu-spacing-real-insert-separtor t)))))

(provide 'init-writing)
