(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package posframe)

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))



(use-package lsp-bridge
  :vc (:fetcher "github"
                :repo "manateelazycat/lsp-bridge")
  :after (posframe markdown-mode)
  :hook (after-init . global-lsp-bridge-mode)
  :custom
  (lsp-bridge-signature-function 'eldoc-message)
  (acm-markdown-render-font-height 80)
  (acm-enable-quick-access t)
  (acm-enable-codeium nil)
  (acm-enable-tabnine nil)
  (acm-backend-yas-match-by-trigger-keyword t)
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (lsp-bridge-python-command "python3"))





(use-package color-rg
  :vc (:fetcher "github"
                :repo "manateelazycat/color-rg"))


(use-package blink-search
  :vc (:fetcher "github"
                :repo "manateelazycat/blink-search")
  :config
  (setq blink-search-search-backends '("Common Directory" "Find File" "IMenu")
        blink-search-common-directory '(("HOME" "~/")
                                        ("ELISP" "~/.emacs.d/lisp/")
                                        ("EMACS" "~/.emacs.d/")
                                        ("PROJECT" "~/projects/"))))


(use-package eat
  :vc (:fetcher "codeberg"
             :repo "akib/emacs-eat"))

(provide 'init-lsp)
