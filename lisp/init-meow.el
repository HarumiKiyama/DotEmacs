;;; init-meow.el -*- lexical-binding: t no-byte-compile: t -*-
(use-package undo-tree
 :diminish
 :init
 (global-undo-tree-mode 1)
 (setq undo-tree-auto-save-history nil))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-goto-line-function 'consult-goto-line)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   )
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("TAB" . meow-last-buffer)
   '("l" . lsp-bridge-list-diagnostics)
   '("r" . lsp-bridge-rename)
   '("d" . lsp-bridge-ref)
   '("k" . kill-this-buffer)
   '(";" . eshell))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-page-down)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("x" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-save)
   '("q" . meow-quit)
   '("Q" . meow-find-ref)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-page-up)
   '("/" . meow-visit)
   '("m" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-yank)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("v" . ignore)
   '("V" . ignore)
   '("e" . meow-mark-word)
   '("E" . meow-mark-symbol)
   )
  )


(use-package meow)
(require 'meow)
(meow-setup)
(meow-setup-indicator)
(meow-global-mode t)

(with-eval-after-load "meow"
  (custom-set-faces
   '(meow-grab ((t (:inherit secondary-selection))))
   '(meow-normal-indicator ((t ())))
   '(meow-motion-indicator ((t ())))
   '(meow-keypad-indicator ((t ())))
   '(meow-insert-indicator ((t ()))))
  (add-to-list 'meow-expand-exclude-mode-list 'magit-mode)
  (add-to-list 'meow-expand-exclude-mode-list 'dired-mode)
  (add-to-list 'meow-expand-exclude-mode-list 'wdired-mode))


;; keybinds
(define-key global-map [remap isearch-forward] 'consult-line)
(define-key global-map [remap isearch-backward] 'consult-ripgrep)
(define-key global-map [remap switch-to-buffer] 'consult-buffer)
(define-key global-map [remap yank-pop] 'consult-yank-pop)

(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map [(tab)] nil)


(provide 'init-meow)
