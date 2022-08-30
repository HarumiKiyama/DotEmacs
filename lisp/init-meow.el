;;; init-meow.el -*- lexical-binding: t no-byte-compile: t -*-
(use-package undo-tree
 :diminish
 :init
 (global-undo-tree-mode 1)
 (setq undo-tree-auto-save-history nil))



(use-package meow)
(meow-global-mode)


;; keybinds
(define-key global-map [remap isearch-forward] 'consult-line)
(define-key global-map [remap isearch-backward] 'consult-ripgrep)
(define-key global-map [remap switch-to-buffer] 'consult-buffer)
(define-key global-map [remap yank] 'consult-yank-from-kill-ring)
(define-key global-map [remap yank-pop] 'consult-yank-pop)

(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map [(tab)] nil)





(provide 'init-meow)
