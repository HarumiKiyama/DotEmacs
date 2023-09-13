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
  (acm-enable-codeium t)
  (acm-enable-tabnine nil)
  (acm-backend-yas-match-by-trigger-keyword t)
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-python-multi-lsp-server nil)
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


;; config tree-sitter
(use-package treesit
  :ensure nil
  :preface
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)
                     (rust-mode . rust-ts-mode)
                     (c++-mode . c++-ts-mode)
                     (c-mode . c-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  )


(use-package realgud)

(use-package realgud-ipdb)

(use-package realgud-lldb)

(provide 'init-lsp)
