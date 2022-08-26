;;; init.el -*- lexical-binding: t no-byte-compile: t -*-
(use-package meow)

(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom))

(global-set-key "\C-s" 'consult-line)
(global-set-key (kbd "s-/") 'hippie-expand)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-c d") 'lispy-describe-inline)
(global-set-key (kbd "C-x ;") 'comment-line)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; r aka remember
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "M-s e") 'iedit-mode)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "s-d") 'harumi/my-mc-mark-next-like-this)
(global-set-key (kbd "C-c l") 'harumi/insert-chrome-current-tab-url)



(use-package general
  :init
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                      nil
                      nil
                      t))

  (general-emacs-define-key 'global [remap imenu] 'consult-imenu)
  (general-emacs-define-key 'global [remap apropos] 'consult-apropos)

  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (global-definer
    "!" 'shell-command
    ":" 'eval-expression
    "SPC" 'execute-extended-command
    "x" 'switch-to-scratch-buffer
    "TAB" 'spacemacs/alternate-buffer
    "'" 'vertico-repeat
    "=" 'indent-buffer
    "u" 'universal-argument
    "v" 'er/expand-region
    "0" 'select-window-0
    "1" 'select-window-1
    "2" 'select-window-2
    "3" 'select-window-3
    ";" 'eshell
    "hdf" 'describe-function
    "hdv" 'describe-variable
    "hdk" 'describe-key
    "qq" 'save-buffers-kill-terminal
    "qR" 'restart-emacs
    "hh" 'harumi/highlight-dwim
    "hc" 'harumi/clearn-highlight
    "sj" 'my/imenu
    "ry" 'consult-yank-pop
    "sp" 'consult-ripgrep
    "oy" 'youdao-dictionary-search-at-point+
    "oo" 'harumi/hotspots
    "or" 'org-roam-node-find
    "gs" 'magit-status
    "gd" 'vc-diff
    "gg" 'xref-find-definitions
    "gr" 'xref-find-references)


  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map ',(intern (concat "+general-global-" name "-map"))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer
    "b" '(switch-to-buffer :which-key "switch buffer")
    "B" '(consult-buffer :which-key "consult buffer")
    "p" 'previous-buffer
    "R" 'rename-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n" 'next-buffer
    "i" 'ibuffer
    "f" 'my-open-current-directory
    "k" 'kill-buffer
    "y" 'copy-buffer-name
    "K" 'kill-other-buffers
    )

  (+general-global-menu! "layout" "l"
    "l" 'persp-switch
    "L" 'persp-state-load
    "TAB" 'persp-switch-last
    "A" 'persp-add-buffer
    "s" 'persp-state-save
    "b" 'persp-switch-to-buffer
    "R" 'persp-remove-buffer
    "k" 'persp-kill)

  (+general-global-menu! "file" "f"
    "f" 'find-file
    "r" 'consult-recent-file
    "L" 'consult-locate
    "d" 'consult-dir
    "ed" 'open-my-init-file
    "s" 'save-buffer
    "w" 'sudo-edit
    "S" 'save-some-buffers
    "j"  'dired-jump
    "y" 'copy-file-name
    "R" 'my/rename-current-buffer-file
    "k" 'my/delete-file-and-buffer
    "!" 'my/exec-shell-on-buffer)

  (+general-global-menu! "window" "w"
    "/" 'split-window-right
    "-" 'split-window-below
    "m" 'delete-other-windows
    "u" 'winner-undo
    "z" 'winner-redo)

  (+general-global-menu! "toggle" "t"
    "s" 'flycheck-mode
    "S" 'flyspell-prog-mode
    "r" 'read-only-mode
    "n" 'my-toggle-line-number
    "w" 'distraction-free
    "k" '+toggle-keycast)

  (+general-global-menu! "project" "p"
    "f" 'project-find-file
    "r" 'consult-recent-file
    "s" 'consult-ripgrep
    "d" 'project-dired
    "b" 'consult-project-buffer
    "e" 'project-eshell
    "m" 'my/project-run-makefile-target
    "c" 'project-compile
    "p" 'project-switch-project
    "i" 'my/project-info
    "a" 'project-remember-projects-under
    "x" 'project-forget-project)

  (general-create-definer global-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
  ;; mode specific major key
  (global-leader
    :major-modes
    '(org-mode t)
    ;;and the keymaps:
    :keymaps
    '(org-mode-map)
    "p" 'org-pomodoro
    "t" 'org-todo
    "e" 'org-set-effort
    ">" 'org-metaright
    "<" 'org-metaleft
    "J" 'org-metadown
    "K" 'org-metaup
    "T" 'org-set-tags-command
    "l" 'org-toggle-link-display
    "L" 'org-toggle-inline-images
    "I" 'org-clock-in
    "O" 'org-clock-out
    "P" 'org-set-property
    "s" 'org-schedule
    "+" 'org-increase-number-at-point
    "-" 'org-decrease-number-at-point
    "n" 'org-narrow-to-subtree
    "dc" 'org-download-clipboard
    "ds" 'org-download-screenshot
    "w" 'widen)

    (global-leader
      :major-modes
      '(org-agenda-mode t)
      ;;and the keymaps:
      :keymaps
      '(org-agenda-mode-map)
      "d" 'org-agenda-day-view
      "w" 'org-agenda-week-view
      "," 'org-agenda-priority
      "e" 'org-agenda-set-effort
      ":" 'org-agenda-set-tags
      "T" 'org-agenda-show-tags)


  )

(provide 'init-keybindings)
