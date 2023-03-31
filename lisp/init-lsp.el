(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :pin nongnu)

(use-package posframe)
(use-package markdown-mode)

(use-package lsp-bridge
  :after (posframe markdown-mode)
  :straight (:host github
                   :repo "manateelazycat/lsp-bridge"
                   :files ("*" (:exclude ".git")))
  :hook (after-init . global-lsp-bridge-mode)
  :custom
  (lsp-bridge-signature-function 'eldoc-message)
  (acm-markdown-render-font-height 80)
  (acm-enable-quick-access t)
  (acm-backend-yas-match-by-trigger-keyword t)
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-python-lsp-server "pyright")
  (lsp-bridge-python-command (let ((home (expand-file-name "~/miniconda3/bin/python"))
                                   (company (expand-file-name "/usr/bin/python")))
                               (if (file-exists-p home)
                                   home
                                 company))))


(use-package color-rg
  :straight (:host github
                   :repo "manateelazycat/color-rg"
                   :files ("*" (:exclude ".git"))))

(use-package blink-search
  :straight (:host github
                   :repo "manateelazycat/blink-search"
                   :files ("*" (:exclude ".git")))
  :config

  (setq blink-search-search-backends '("Common Directory" "Find File" "IMenu")
        blink-search-common-directory '(("HOME" "~/")
                                        ("EMACS PACKAGE" "~/.emacs.d/lisp")
                                        ("EMACS" "~/.emacs.d")
                                        ("PROJECT" "~/projects"))))

(use-package aweshell
  :straight (:host github
                   :repo "manateelazycat/aweshell"
                   :files ("*" (:exclude ".git"))))

(use-package nova
  :straight (:host github
                   :repo "manateelazycat/nova"
                   :files ("*" (:exclude ".git"))))


(provide 'init-lsp)
