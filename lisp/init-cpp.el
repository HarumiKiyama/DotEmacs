(use-package cmake-mode)

(use-package cc-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode)))

(provide 'init-cpp)
