
(use-package proof-general
  :ensure t
  :init
  (setq proof-three-window-mode-policy 'hybrid
        proof-script-fly-past-comments t
        proof-splash-seen t))

(use-package company-coq
  :ensure t
  :hook (coq-mode . company-coq-mode))

(provide 'init-coq)
