(use-package blacken
  :commands blacken-buffer
  :bind (:map python-mode-map
              ("C-x = =" . blaken-buffer)))

(use-package pytest)

(use-package pyvenv)

(use-package py-isort
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-x = i" . py-isort-buffer)))

(provide 'init-python)
