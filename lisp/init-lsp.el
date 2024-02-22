;; config tree-sitter
(use-package treesit
  :ensure nil
  :preface
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (go-mode . go-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)
                     (sh-mode . bash-ts-mode)
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
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"))))




(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (advice-add 'yas--modes-to-activate :around
              (defun yas--get-snippet-tables@reverse-ts-mode (orig-fn &optional mode)
                (funcall orig-fn
                         (or (car (rassq (or mode major-mode) major-mode-remap-alist))
                             mode))))

  ;; 在 *-ts-mode 下保存 snippets 到 *-mode 目录 （也可以试试直接在 yas--table-get-create 上 advice 看看有没有不良影响）
  (advice-add 'yas-new-snippet :around
              (defun yas-new-snippet@reverse-ts-mode (&rest args)
                (cl-letf* ((yas--orig-table-get-create (symbol-function 'yas--table-get-create))
                           ((symbol-function 'yas--table-get-create)
                            (lambda (mode)
                              (funcall yas--orig-table-get-create
                                       (or (car (rassq (or mode major-mode) major-mode-remap-alist))
                                           mode)))))
                  (apply args))))
  (yas-global-mode 1))

(use-package posframe)

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))


(use-package lsp-bridge
  :vc (:fetcher "github"
                :repo "manateelazycat/lsp-bridge")
  :after (posframe markdown-mode)
  :hook (after-init . global-lsp-bridge-mode)
  :config
  (setq lsp-bridge-signature-function 'eldoc-message
        acm-markdown-render-font-height 80
        acm-enable-quick-access t
        acm-backend-yas-match-by-trigger-keyword t
        lsp-bridge-code-action-enable-popup-menu nil
        lsp-bridge-python-multi-lsp-server "pyright_ruff"
        lsp-bridge-python-command "python3"))


(use-package color-rg
  :vc (:fetcher "github"
                :repo "manateelazycat/color-rg")
  )


(use-package blink-search
  :vc (:fetcher "github"
                :repo "manateelazycat/blink-search")
  :config
  (setq blink-search-search-backends '("Common Directory" "Find File" "IMenu")
        blink-search-common-directory '(("HOME" "~/")
                                        ("ELISP" "~/.emacs.d/lisp/")
                                        ("EMACS" "~/.emacs.d/")
                                        ("PROJECT" "~/projects/"))))


(use-package realgud)
(use-package realgud-ipdb)
(use-package realgud-lldb)

(provide 'init-lsp)
