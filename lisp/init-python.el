(use-package python-mode
  :config
  (setq py-shell-name "ipython")
  :bind (:map python-mode-map
              ("C-c =" . nil)))

(require 'python-mode)

(use-package pytest)

(use-package pyvenv)

(use-package conda)

(use-package py-isort
  :defer t
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-c = i" . py-isort-buffer)))

(use-package blacken
  :defer t
  :commands blacken-buffer
  :bind (:map python-mode-map
              ("C-c = =" . blaken-buffer)))

(provide 'init-python)
