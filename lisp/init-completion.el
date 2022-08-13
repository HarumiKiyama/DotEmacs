;;;;  -*- lexical-binding: t; -*-
(require 'init-funcs)

(use-package all-the-icons
  :ensure t)

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-'") 'vertico-quick-jump)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "C-M-SPC") #'+vertico/embark-preview))

(use-package consult
  :ensure t
  :defer t
  :init
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'consult-line
              :around
              #'harumi/consult-line
              '((name . "wrapper")))
  :config
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (setq ;; consult-project-root-function #'doom-project-root
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay 0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   :preview-key (kbd "C-SPC"))
  (consult-customize
   consult-theme
   :preview-key (list (kbd "C-SPC") :debounce 0.5 'any)))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-M-d" . consult-dir)))

(use-package consult-flycheck
  :after (consult flycheck))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  :config)

(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
	;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
	prefix-help-command #'embark-prefix-help-command)

  (setq
   embark-verbose-indicator-display-action
   '((display-buffer-at-bottom)
     (window-parameters (mode-line-format . none))
     (window-height . fit-window-to-buffer)))

  (define-key minibuffer-local-map (kbd "C-;") 'embark-act)
  (define-key minibuffer-local-map (kbd "C-c C-;") 'embark-export)
  (define-key minibuffer-local-map (kbd "C-c C-e") '+vertico/embark-export-write)
  (with-eval-after-load 'popwin
    (progn
      (push '(occur-mode :position right :width 100) popwin:special-display-config)
      (push '(grep-mode :position right :width 100) popwin:special-display-config)
      (push '(special-mode :position right :width 100) popwin:special-display-config)))
  (global-set-key (kbd "C-;") 'embark-act)
  :config
  (define-key minibuffer-local-map (kbd "C-'") #'embark-become)
  ;; list all the keybindings in this buffer
  (global-set-key (kbd "C-h B") 'embark-bindings)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  :config
  (define-key embark-identifier-map "R" #'consult-ripgrep)
  (define-key embark-identifier-map (kbd "C-s") #'consult-line)
  (define-key embark-file-map (kbd "E") #'consult-directory-externally)
  (define-key embark-file-map (kbd "U") #'consult-snv-unlock))


(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package yasnippet
  :ensure t)
(use-package yasnippet-snippets
  :ensure t)

(use-package dumb-jump
  :ensure t)

;; lsp-bridge config
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


(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)   ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))

(evil-add-command-properties #'lsp-bridge-jump :jump t)
(setq-local evil-goto-definition-functions '(lsp-bridge-jump))
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
(define-key lsp-bridge-mode-map (kbd "C-M-j") 'lsp-bridge-popup-documentation-scroll-down)
(define-key lsp-bridge-mode-map (kbd "C-M-k") 'lsp-bridge-popup-documentation-scroll-up)
(define-key acm-mode-map (kbd "C-j") 'acm-select-next)
(define-key acm-mode-map (kbd "C-k") 'acm-select-prev)


(provide 'init-completion)
