;;; init-lsp.el -*- lexical-binding: t no-byte-compile: t -*-




(defun my-eglot-keybindgs ()
  (define-key evil-motion-state-map "gR" #'eglot-rename)
  (define-key evil-motion-state-map "gr" #'xref-find-references)
  (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
  (define-key evil-motion-state-map "gh" #'eldoc)
  (define-key evil-normal-state-map "ga" #'eglot-code-actions))

(use-package eglot
  :ensure t
  :init
  (advice-add 'eglot-ensure :after 'my-eglot-keybindgs)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("s-<return>" . eglot-code-actions))
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.2)
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p nil))


(require 'lsp-bridge)

(setq lsp-bridge-enable-log nil)

(defun my/enable-lsp-bridge ()
  (interactive)
  (progn
    (corfu-mode -1)
    (lsp-bridge-mode)

    (setq-local evil-goto-definition-functions '(lsp-bridge-jump))
    (setq acm-candidate-match-function 'orderless-flex)

    (define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
    (define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
    (define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
    (define-key evil-motion-state-map "gd" #'lsp-bridge-jump)
    (define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
    (define-key evil-normal-state-map "gh" #'lsp-bridge-lookup-documentation)
    (define-key evil-normal-state-map "gn" #'lsp-bridge-jump-to-next-diagnostic)
    (define-key evil-normal-state-map "gp" #'lsp-bridge-jump-to-prev-diagnostic)
    (define-key evil-normal-state-map "ga" #'lsp-bridge-code-action)
    (define-key evil-normal-state-map "ge" #'lsp-bridge-list-diagnostics)

    (define-key lsp-bridge-mode-map (kbd "s-j") 'lsp-bridge-popup-documentation-scroll-down)
    (define-key lsp-bridge-mode-map (kbd "s-k") 'lsp-bridge-popup-documentation-scroll-up)
    (define-key acm-mode-map (kbd "C-j") 'acm-select-next)
    (define-key acm-mode-map (kbd "C-k") 'acm-select-prev)
    ))


(use-package dumb-jump
  :ensure t)
(evil-add-command-properties #'lsp-bridge-jump :jump t)



;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (evil-goto-definition))
   ((eq major-mode 'org-mode)
    (org-agenda-open-link))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

(evil-define-key 'normal lsp-bridge-ref-mode-map
  (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
  "q" 'lsp-bridge-ref-quit)

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'init-lsp)
