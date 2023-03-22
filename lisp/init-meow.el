;;; init-meow.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))))


(use-package meow
  :custom
  (meow-use-clipboard t)
  (meow-goto-line-function 'avy-goto-line)
  :config
  (meow-global-mode)
  (add-to-list 'meow-mode-state-list '(elfeed-show-mode . normal))
  (add-to-list 'meow-mode-state-list '(elfeed-summary-mode . motion))
  (add-to-list 'meow-mode-state-list '(helpful-mode . normal))
  (add-to-list 'meow-mode-state-list '(calibredb-search-mode . motion))
  (add-to-list 'meow-mode-state-list '(Info-mode-hook . motion))

  (require 'one-key)
  (setq one-key-popup-window nil)
  (one-key-create-menu
   "DIRECTORY"
   '((("d" . "Downloads") . (lambda () (interactive) (dired "~/Downloads/")))
     (("i" . "Init") . (lambda () (interactive) (dired "~/.emacs.d/lisp/")))
     (("b" . "Books") . (lambda () (interactive) (dired "~/Desktop/books"))))
   t)
  (meow-leader-define-key '("d" . one-key-menu-directory))
  (one-key-create-menu
   "ORG"
   '((("p" . "Pomodoro") . org-pomodoro)
     (("a" . "Agenda") . org-agenda-list)
     (("l" . "cliplink") . org-cliplink))
   t)
  (meow-leader-define-key '("o" . one-key-menu-org))

  (one-key-create-menu
   "WINDOWS"
   '((("d" . "Destroy") . kill-this-buffer)
     (("h" . "Left") . windmove-left)
     (("j" . "Down") . windmove-down)
     (("k" . "Up") . windmove-up)
     (("l" . "Right") . windmove-right)
     (("a" . "ace") . ace-window)
     (("c" . "Chat") . erc)
     (("r" . "RSS") . elfeed)
     (("m" . "Message") . (lambda () (interactive) (switch-to-buffer "*Messages*")))
     (("s" . "scratch") . (lambda () (interactive) (switch-to-buffer "*scratch*")))
     (("u" . "Winner Undo") . winner-undo))
   t)

  (one-key-create-menu
   "LSP"
   '((("r" . "Rename") . lsp-bridge-rename)
     (("n" . "Next") . lsp-bridge-diagnostic-jump-next)
     (("p" . "Prev") . lsp-bridge-diagnostic-jump-prev)
     (("l" . "List") . lsp-bridge-diagnostic-list)
     (("a" . "Action") . lsp-bridge-code-action)
     (("R" . "reference") . lsp-bridge-find-references)
     (("d" . "Def") . lsp-bridge-jump)))


  (one-key-create-menu
   "RG"
   '(
     (("d" . "Search Directory") . color-rg-search-input)
     (("f" . "Search file") . color-rg-search-input-in-current-file)
     (("p" . "Search Project") . color-rg-search-input-in-project)))

  (one-key-create-menu
   "FILE"
   '(
     (("r" . "Rename File") . harumi/rename-file-and-buffer)
     (("d" . "delete File") . harumi/delete-file-and-buffer)
     (("D" . "Dos2Unix") . dos2unix)
     (("U" . "Unix2Dos") . unix2dos)
     (("M" . "DELETE^M") . delete-carrage-returns)))
  (meow-leader-define-key '("l" . one-key-menu-lsp))
  (meow-leader-define-key '("w" . one-key-menu-windows))
  (meow-leader-define-key '("r" . one-key-menu-rg))
  (meow-leader-define-key '("f" . one-key-menu-file))


  ;; keybinds
  (define-key global-map [remap isearch-forward] 'consult-line)
  (define-key global-map [remap isearch-backward] 'blink-search)
  (define-key global-map [remap switch-to-buffer] 'consult-buffer)
  (define-key global-map [remap goto-line] 'consult-goto-line)
  (define-key global-map [remap goto-char] 'avy-goto-char)
  (define-key global-map [remap yank-pop] 'consult-yank-pop)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "M-'") 'yas-expand)
  (define-key global-map (kbd "C-x (") 'meow-beacon-start)
  (define-key global-map (kbd "C-x )") 'meow-beacon-end-and-apply-kmacro))

(require 'meow)

(defun meow-setup ()
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("SPC" . set-mark-command)
   '("k" . "H-k")
   '("s" . persp-switch)
   '("TAB" . meow-last-buffer)
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
   '("?" . meow-cheatsheet))
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
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-char)
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
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-line)
   '("n" . meow-open-below)
   '("N" . meow-open-above)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-page-up)
   '("e" . meow-mark-word)
   '("E" . meow-mark-symbol)
   '("V" . meow-search)
   '("v" . meow-visit)
   '("x" . meow-goto-line)
   '("X" . meow-join)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))


(provide 'init-meow)
