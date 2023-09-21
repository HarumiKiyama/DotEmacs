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


(use-package combobulate
  :vc (:fetcher github
                :repo "mickeynp/combobulate"))


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

(require 'pretty-hydra)


(pretty-hydra-define leader-org-hydra
  (:hint nil :color blue :quit-key "q" :title "ORG")
  ("GTD"
   (("a" org-agenda-list "agenda")
    ("p" org-pomodoro "pomodoro"))
   "NOTE"
   (
    ("c" org-capture "capture")
    ("d" org-journal-new-entry "diary"))))

(pretty-hydra-define leader-paren-hydra (:hint nil :color blue :quit-key "q" :title "PAREN")
  ("EDIT"
   (("c" sp-rewrap-sexp "change")
    ("d" sp-unwrap-sexp "delete"))
   "NAV"
   (("f" sp-forward-sexp "forward" :exit nil)
    ("b" sp-backward-sexp "backward" :exit nil)
    ("p" sp-up-sexp "up" :exit nil)
    ("n" sp-down-sexp "down" :exit nil)
    )))

(pretty-hydra-define leader-windows-hydra (:hint nil :color blue :quit-key "q" :title "WINDOWS")
  ("MODIFY"
   (("d" kill-this-buffer "kill")
    ("-" split-window-below "Split below")
    ("/" split-window-right "Split right")
    ("u" winner-undo "Winner Undo"))
   "SWITCH"
   (("s" scratch-buffer "scratch")
    ("m" message-buffer)
    )))


(pretty-hydra-define leader-lsp-hydra (:hint nil :color blue :quit-key "q" :title "LSP")
  ("NAV"
   (
    ("n" lsp-bridge-diagnostic-jump-next "next" :exit nil)
    ("p" lsp-bridge-diagnostic-jump-prev "prev" :exit nil)
    ("s" lsp-bridge-peek "select")
    ("l" lsp-bridge-diagnostic-list "list error")
    ("R" lsp-bridge-find-references "ref")
    ("d" lsp-bridge-jump "def"))
   "ACTION"
   (("r" lsp-bridge-rename "rename")
    ("a" lsp-bridge-code-action "action"))))


(pretty-hydra-define leader-file-hydra (:hint nil :color blue :quit-key "q" :title "FILE")
  ("FILE"
   (("r" rename-visited-file "rename")
    ("d" harumi/delete-file-and-buffer "delete"))
   "CONTENT"
   (
    ("u" dos2unix)
    ("U" unix2dos)
    ("M" delete-carrage-returns "delete ^M"))))

(pretty-hydra-define leader-search-hydra (:hint nil :color blue :quit-key "q" :title "SEARCH")
  ("GLOBAL"
   (("b" consult-bookmark "bookmark")
    ("t" tab-switch "tab"))
   "Project"
   (
    ("d" color-rg-search-input "dir")
    ("p" color-rg-search-input-in-project "project"))

   "FILE"
   (
    ("c" avy-goto-char "char")
    ("l" consult-goto-line "line")
    ("f" color-rg-search-input-in-current-file "file"))))

(pretty-hydra-define leader-tool-hydra (:hint nil :color blue :quit-key "q")
  ("TOOL"
   (("e" elfeed)
    ("c" calibredb)
    ("s" hydra-smerge/body "smerge")
    ("c" erc))))

(pretty-hydra-define leader-tab-hydra (:hint nil :color blue :quit-key "q" :title "TAB")
  ("NAV"
   (
    ("n" tab-next "next" :exit nil)
    ("p" tab-previous "previous" :exit nil))
   "ACTION"
   (
    ("d" tab-close :exit nil))))

(defun meow--select-expandable-p ()
  (when (meow-normal-mode-p)
    (when-let ((sel (meow--selection-type)))
      (let ((type (cdr sel)))
        (member type '(word line block find till fun))))))
  

(defun meow-setup ()
  (meow-leader-define-key
   '("f" . leader-file-hydra/body)
   '("w" . leader-windows-hydra/body)
   '("l" . leader-lsp-hydra/body)
   '("o" . leader-org-hydra/body)
   '("t" . leader-tab-hydra/body)
   '("T" . leader-tool-hydra/body)
   '("p" . leader-paren-hydra/body)
   '("s" . leader-search-hydra/body)
   '("m" . set-mark-command)
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
   '("Z" . leader-search-hydra/body)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :after (recentf consult)
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
                                 (smerge-mode . motion)
                                 (blink-search-mode . insert)
                                 (color-rg-mode . insert)
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
  (keymap-global-set "C-o" 'major-mode-hydra)


  (keymap-unset yas-minor-mode-map "TAB")
  (keymap-unset yas-minor-mode-map "<tab>")
  (keymap-set yas-minor-mode-map "M-'" 'yas-expand)

  ;; register thing
  (meow-thing-register 'ts-fun #'meow-ts--get-defun-at-point #'meow-ts--get-defun-at-point)
  (add-to-list 'meow-char-thing-table '(?f . ts-fun)))

(provide 'init-meow)
