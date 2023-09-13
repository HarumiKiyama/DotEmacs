;;; init-meow.el -*- lexical-binding: t no-byte-compile: t -*-
(require 'dash)

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

(use-package one-key
  :vc (:fetcher github
                :repo "manateelazycat/one-key")
  :custom
  (one-key-popup-window t))


(use-package combobulate
  :vc (:fetcher github
                :repo "mickeynp/combobulate")
  )


(defun meow-ts--get-defun-at-point ()
  (let ((node (treesit-defun-at-point)))
    `(,(treesit-node-start node) . ,(treesit-node-end node))
    ))

;; TODO meow next / previous defun (just like words) - also make an expandable defun too!

;; TODO meow next / previous defun (just like words) - also make an expandable defun too!
(defun meow-ts-next-defun (n)
  "Select to the end of the next Nth function(tree-sitter).
A non-expandable, function selection will be created."
  (interactive "p")
  (unless (equal 'fun (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (let* ((expand (equal '(expand . fun) (meow--selection-type)))
         (_ (when expand (meow--direction-forward)))
         (type (if expand '(expand . fun) '(select . fun)))
         (m (or (save-mark-and-excursion
		          (treesit-end-of-defun n)
		          (when (treesit-beginning-of-defun)
		            (point)))
		        (point)))
         (p (save-mark-and-excursion
              (treesit-end-of-defun n)
              (point)
              )))
    (when p
      (thread-first
        (meow--make-selection type m p expand)
        (meow--select))
      ;; this requires modifying `meow--select-expandable-p' - to include the "fun" seletion type as expandable
      )))


(defun meow-ts--forward-defun-1 ()
  (treesit-end-of-defun 1)
  (point))

(defun meow-ts--backward-defun-1 ()
  (treesit-beginning-of-defun 1)
  (point))


(defun meow-next-defun (n)
  (interactive "p")
  (if (and (featurep 'treesit) (treesit-available-p) (treesit-parser-list))
      (meow-ts-next-defun n)
    (meow-block n)))



(defun meow-setup ()
  (defun meow--select-expandable-p ()
    (when (meow-normal-mode-p)
      (when-let ((sel (meow--selection-type)))
        (let ((type (cdr sel)))
          (member type '(word line block find till fun))))))
  
  (one-key-create-menu
   "PAREN"
   '((("c" . "Change paren") . sp-rewrap-sexp)
     (("d" . "Delete paren") . sp-unwrap-sexp))
   t)
  
  (one-key-create-menu
   "ORG"
   '((("p" . "Pomodoro") . org-pomodoro)
     (("c" . "Capture") . org-capture)
     (("d" . "Diary") . org-journal-new-entry)
     (("a" . "Agenda") . org-agenda-list))
   t)

  (one-key-create-menu
   "WINDOWS"
   '((("d" . "Destroy") . kill-this-buffer)
     (("-" . "Split below") . split-window-below)
     (("/" . "Split right") . split-window-right)
     (("c" . "Chat") . erc)
     (("r" . "RSS") . elfeed)
     (("m" . "Message") . (lambda () (interactive) (switch-to-buffer "*Messages*")))
     (("s" . "scratch") . scratch-buffer)
     (("u" . "Winner Undo") . winner-undo))
   t)

  (one-key-create-menu
   "LSP"
   '((("r" . "Rename") . lsp-bridge-rename)
     (("n" . "Next") . lsp-bridge-diagnostic-jump-next)
     (("p" . "Prev") . lsp-bridge-diagnostic-jump-prev)
     (("s" . "Select") . lsp-bridge-peek)
     (("l" . "List") . lsp-bridge-diagnostic-list)
     (("a" . "Action") . lsp-bridge-code-action)
     (("R" . "reference") . lsp-bridge-find-references)
     (("d" . "Def") . lsp-bridge-jump)))

  (one-key-create-menu
   "FILE"
   '((("r" . "Rename File") . rename-visited-file)
     (("d" . "delete File") . harumi/delete-file-and-buffer)
     (("u" . "Dos2Unix") . dos2unix)
     (("U" . "Unix2Dos") . unix2dos)
     (("R" . "run script") . quickrun)
     (("M" . "DELETE^M") . delete-carrage-returns)))

  (one-key-create-menu
   "SEARCH"
   '((("b" . "Bookmark") . consult-bookmark)
     (("t" . "tab") . tab-switch)
     (("P" . "Project") . consult-project-buffer)
     (("c" . "Char") . avy-goto-char)
     (("l" . "Line") . consult-goto-line)
     (("d" . "Search Directory") . color-rg-search-input)
     (("f" . "Search file") . color-rg-search-input-in-current-file)
     (("p" . "Search Project") . color-rg-search-input-in-project)))

  (one-key-create-menu
   "TOOL"
   '((("e" . "elfeed") . elfeed)
     (("c" . "calibre") . calibredb)))
  (one-key-create-menu
   "TAB"
   '((("d" . "delete") . tab-close)
     (("D" . "delete other") . tab-close-other)
     (("s" . "tab switch") . tab-switch)
     (("g" . "group") . tab-group)
     (("n" . "new") . tab-new)
     (("l" . "left") . tab-next)
     (("h" . "right") . tab-previous)))
  
  (meow-leader-define-key
   '("f" . one-key-menu-file)
   '("w" . one-key-menu-windows)
   '("l" . one-key-menu-lsp)
   '("o" . one-key-menu-org)
   '("t" . one-key-menu-tab)
   '("T" . one-key-menu-tool)
   '("p" . one-key-menu-paren)
   '("m" . set-mark-command)
   '("s" . one-key-menu-search)
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
   '("A" . quickrun)
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
   '("I" . consult-imenu)
   '("j" . meow-next)
   '("J" . meow-next-expand)            
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-line)
   '("n" . meow-open-below)
   '("N" . meow-open-above)
   ;; '("o" . meow-block)
   '("o" . meow-next-defun)
   '("O" . tabspaces-open-or-create-project-and-workspace)
   '("p" . meow-yank)
   '("P" . tabspaces-switch-or-create-workspace)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("S" . eat)
   '("t" . meow-till)
   '("T" . find-file-other-tab)
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
   '("Z" . one-key-menu-search)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :after (one-key recentf consult)
  :custom
  (meow-use-clipboard t)
  (meow-goto-line-function 'avy-goto-line)
  (meow-expand-hint-remove-delay 5.0)
  :init
  (meow-global-mode)
  (meow-setup)
  (setq custom-mode-state-list '((elfeed-show-mode . motion)
                                 (elfeed-summary-mode . motion)
                                 (helpful-mode . motion)
                                 (blink-search-mode . motion)
                                 (color-rg-mode . motion)
                                 (lsp-bridge-ref-mode . motion)
                                 (Info-mode-hook . motion)
                                 (calibredb-show-mode-hook . motion)
                                 (calibredb-search-mode-hook . motion)))
  (setq meow-mode-state-list (-concat meow-mode-state-list custom-mode-state-list))

  ;; keybinds
  (keymap-substitute global-map 'isearch-forward 'consult-line)
  (keymap-substitute global-map 'isearch-backward 'blink-search)
  (keymap-substitute global-map 'Info-search 'consult-info)
  (keymap-substitute global-map 'switch-to-buffer 'consult-buffer)
  (keymap-substitute global-map 'list-buffers 'ibuffer-list-buffers)
  (keymap-substitute global-map 'goto-line 'consult-goto-line) 
  (keymap-substitute global-map 'goto-char 'avy-goto-char)
  (keymap-substitute global-map 'yank-pop 'consult-yank-pop)
  (keymap-substitute global-map 'bookmark-jump 'consult-bookmark)
  (keymap-substitute global-map 'other-window 'ace-window)
  (keymap-substitute global-map 'recentf-open-files 'consult-recent-file)

  (keymap-global-set "C-x f" 'consult-project-buffer)
  (keymap-global-set "C-x (" 'meow-start-kmacro-or-insert-counter)
  (keymap-global-set "C-x )" 'meow-end-or-call-kmacro)

  (keymap-unset yas-minor-mode-map "TAB")
  (keymap-unset yas-minor-mode-map "<tab>")
  (keymap-set yas-minor-mode-map "M-'" 'yas-expand)

  ;; register thing
  (meow-thing-register 'ts-fun #'meow-ts--get-defun-at-point #'meow-ts--get-defun-at-point)
  (add-to-list 'meow-char-thing-table '(?f . ts-fun)))

(provide 'init-meow)
