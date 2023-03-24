(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :pin nongnu)

(use-package posframe)
(use-package markdown-mode)
(require 'lsp-bridge)
;; (add-to-list
;;  'lsp-bridge-single-lang-server-mode-list '(idris2-mode . "idris2-lsp"))
;; (add-to-list 'lsp-bridge-default-mode-hooks 'idris2-mode-hook)
(setq lsp-bridge-python-command (let ((home (expand-file-name "~/miniconda3/bin/python"))
                                      (company (expand-file-name "/usr/bin/python")))
                                  (if (file-exists-p home)
                                      home
                                    company))
      acm-enable-quick-access t
      acm-backend-yas-match-by-trigger-keyword t
      lsp-bridge-code-action-enable-popup-menu nil
      lsp-bridge-python-multi-lsp-server "pyright_ruff"
      lsp-bridge-python-lsp-server "pyright")

(global-lsp-bridge-mode)

(provide 'init-lsp)
