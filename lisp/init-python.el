(use-package py-isort
  :defer t
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-c = i" . py-isort-buffer)))

(use-package blacken
  :defer t
  :commands blacken-buffer
  :bind (:map python-mode-map
              ("C-c = =" . blacken-buffer)))

(provide 'init-python)
