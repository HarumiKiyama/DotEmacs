;;; init-meow.el -*- lexical-binding: t no-byte-compile: t -*-
(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))


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
   '((("c" . "Capture") . one-key-menu-org-capture)
     (("d" . "Download") . one-key-menu-download)
     (("p" . "Process") . org-gtd-process-inbox)
     (("P" . "Pomodoro") . org-pomodoro)
     (("l" . "cliplink") . org-cliplink)
     (("i" . "ID") . org-id-get-create)
     (("t" . "Transclusion") . one-key-menu-transclusion))
   t)

  (meow-leader-define-key '("o" . one-key-menu-org))

  (one-key-create-menu
   "DOWNLOAD"
   '((("c" . "clipboard") . org-download-clipboard)
     (("i" . "image") . org-download-image)
     (("r" . "rename") . org-download-rename-at-point)
     (("s" . "screenshot") . org-download-screenshot))
   t)

  (meow-leader-define-key '("l" . one-key-menu-language))

  (one-key-create-menu
   "ROAM"
   '((("a" . "Alias") . one-key-menu-roam-alias)
     (("c" . "Roam capture") . org-roam-capture)
     (("d" . "Dailies") . one-key-menu-roam-dailies)
     (("f" . "Node find") . org-roam-node-find)
     (("i" . "Node insert") . org-roam-node-insert)
     (("t" . "Tags") . one-key-menu-roam-tags)
     (("r" . "Ref") . one-key-menu-roam-ref)
     (("u" . "UI") . one-key-menu-roam-ui))
   t)

  (meow-leader-define-key '("r" . one-key-menu-roam))

  (one-key-create-menu
   "ROAM-ALIAS"
   '((("a" . "Add") . org-roam-alias-add)
     (("r" . "Remove") . org-roam-alias-remove))
   t)

  (one-key-create-menu
   "ROAM-DAILIES"
   '((("t" . "Today") . org-roam-dailies-find-today)
     (("y" . "Yesterday") . org-roam-dailies-find-yesterday)
     (("d" . "Directory") . org-roam-dailies-find-directory)
     (("j" . "Date") . org-roam-dailies-goto-date))
   t)

  (one-key-create-menu
   "ROAM-REF"
   '((("a" . "Add") . org-roam-ref-add)
     (("f" . "Find") . org-roam-ref-find)
     (("r" . "Remove") . org-roam-ref-remove))
   t)

  (one-key-create-menu
   "ROAM-TAGS"
   '((("a" . "Add") . org-roam-tag-add)
     (("c" . "Completion") . org-roam-tag-completions)
     (("r" . "Remove") . org-roam-tag-remove))
   t)

  (one-key-create-menu
   "ROAM-UI"
   '((("o" . "Open") . org-roam-ui-open)
     (("l" . "Local") . org-roam-ui-node-local)
     (("z" . "Zome") . org-roam-ui-node-zoom)))

  (one-key-create-menu
   "WINDOWS"
   '((("d" . "Destroy") . kill-this-buffer)
     (("h" . "Left") . windmove-left)
     (("j" . "Down") . windmove-down)
     (("k" . "Up") . windmove-up)
     (("l" . "Right") . windmove-right)
     (("e" . "Email") . mu4e)
     (("r" . "RSS") . elfeed-summary)
     (("t" . "Telega") . telega)
     (("m" . "Message") . (lambda () (interactive) (switch-to-buffer "*Messages*")))
     (("s" . "scratch") . (lambda () (interactive) (switch-to-buffer "*scratch*")))
     (("u" . "Winner Undo") . winner-undo))
   t)

  (meow-leader-define-key '("w" . one-key-menu-windows))

  (one-key-create-menu
   "TOOLS"
   '((("o" . "Outline") . consult-outline)
     (("e" . "elfeed") . elfeed)
     (("c" . "chat") . erc))
   t)

  (meow-leader-define-key '("t" . one-key-menu-tools))

  ;; keybinds
  (define-key global-map [remap isearch-forward] 'consult-line)
  (define-key global-map [remap isearch-backward] 'blink-search)
  (define-key global-map [remap switch-to-buffer] 'consult-buffer)
  (define-key global-map [remap yank-pop] 'consult-yank-pop)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "M-'") 'yas-expand))
(require 'meow)

(defun meow-setup ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   '("s" . persp-switch)
   '("D" . lsp-bridge-find-def)
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
