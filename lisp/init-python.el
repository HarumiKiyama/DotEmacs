(use-package blacken
  :commands blacken-buffer
  :bind (:map python-mode-map
              ("C-c = =" . blaken-buffer)))

(use-package pytest)

(use-package pyvenv)

(use-package conda)

(use-package py-isort
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-c = i" . py-isort-buffer)))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--pylab"
      python-shell-completion-native-enable nil)

(provide 'init-python)
