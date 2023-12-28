(use-package py-isort
  :commands py-isort-buffer)

(use-package blacken
  :commands blacken-buffer)


(major-mode-hydra-define python-ts-mode (:color blue :hint nil :quit-key "q")
  ("FORMAT"
   (("b" blacken-buffer "buffer")
    ("i" py-isort-buffer "import"))))

(use-package jupyter)


(provide 'init-python)
