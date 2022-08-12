;;; init-lsp.el -*- lexical-binding: t no-byte-compile: t -*-
(require 'lsp-bridge)
(require 'corfu-info)
(require 'corfu-history)

(setq lsp-bridge-enable-log nil)

(defun my/enable-lsp-bridge ()
  (interactive)
  (progn
    (lsp-bridge-mode)
    (flycheck-mode)
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
    (define-key acm-mode-map (kbd "C-k") 'acm-select-prev)))


(add-hook 'rust-mode-hook 'my/enable-lsp-bridge)
(add-hook 'python-mode-hook 'my/enable-lsp-bridge)
;; (add-hook 'haskell-mode-hook 'my/enable-lsp-bridge)
;; (add-hook 'idris-mode-hook 'my/enable-lsp-bridge)


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
