;;; init.el -*- lexical-binding: t no-byte-compile: t -*-


(require 'cl-lib)
(when (not (version< emacs-version "29.0"))
  (setq package-native-compile nil))

;; speed up startup
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            "recover gc values after startup."
            (setq gc-cons-threshold 80000000
                  gc-cons-percentage 0.1)))

;; load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)
(update-load-path)

;; packages
(require 'init-package)

;; config path env
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; config lsp-brigle
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :pin nongnu)
(use-package posframe)
(use-package markdown-mode)
(require 'lsp-bridge)
;; (add-to-list
;;  'lsp-bridge-single-lang-server-mode-list '(idris2-mode . "idris2-lsp"))
;; (add-to-list 'lsp-bridge-default-mode-hooks 'idris2-mode-hook)
(setq lsp-bridge-python-command (let ((home (expand-file-name "~/miniconda3/bin/python"))
                                      (company (expand-file-name "/usr/bin/python")))
                                  (if (file-exists-p home)
                                      home
                                    company))
      acm-enable-quick-access t
      acm-backend-yas-match-by-trigger-keyword t
      lsp-bridge-code-action-enable-popup-menu nil
      lsp-bridge-python-multi-lsp-server "pyright_ruff"
      lsp-bridge-python-lsp-server "pyright")
(global-lsp-bridge-mode)

;; config meow
(require 'init-meow)
(add-hook 'after-init-hook 'meow-setup)
;; config blink-search
(require 'blink-search)
;; config color-rg
(require 'color-rg)
;; config aweshell
(require 'aweshell)

;; core
(require 'init-basic)
(require 'init-tools)
(require 'init-writing)


;; uis
(require 'init-ui)
(require 'init-window)
(require 'init-better-defaults)

;; tools
(require 'init-org)
(require 'init-git)
(require 'init-elfeed)
(require 'init-irc)
(require 'init-completion)

;; frameworks
(require 'init-persp)
;; languages
(require 'init-lisp)
(require 'init-rust)
(require 'init-coq)
(require 'init-python)
;; (require 'init-go)
;; (require 'init-idris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
