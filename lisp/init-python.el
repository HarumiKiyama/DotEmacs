(use-package blacken
  :commands blacken-buffer
  :bind (:map python-mode-map
              ("C-c = =" . blaken-buffer)))

(use-package python-mode
  :config
  (setq py-shell-name "ipython"))

(use-package pytest)

(use-package pyvenv)

(use-package conda)

(use-package py-isort
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-c = i" . py-isort-buffer)))

(provide 'init-python)
