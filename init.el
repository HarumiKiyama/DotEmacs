;;; init.el -*- lexical-binding: t no-byte-compile: t -*-
(require 'cl-lib)

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

(menu-bar-mode -1)

;; packages
(require 'init-package)

;; config path env
;; (use-package exec-path-from-shell)
;; (exec-path-from-shell-initialize)

;; config lsp
(require 'init-basic)
(require 'init-lsp)
(require 'init-meow)


;; ui
(require 'init-ui)
(require 'init-window)
(require 'init-better-defaults)
(require 'init-workspaces)

;; tools
(require 'init-tools)
(require 'init-org)
(require 'init-git)
(require 'init-server)
(require 'init-writing)

;; (require 'init-irc)
(require 'init-completion)

;; languages
(require 'init-lisp)
(require 'init-rust)
(require 'init-python)
(require 'init-cpp)
(require 'init-go)

;; (require 'init-coq)
;; (require 'init-idris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
