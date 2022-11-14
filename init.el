
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
;; without this comment emacs25 adds (package-initialize) here
(require 'init-package)

(use-package benchmark-init
  :ensure t
  :demand t
  :config
  ;; to disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(require 'yasnippet)
(yas-global-mode 1)
(require 'lsp-bridge)
(add-to-list
 'lsp-bridge-single-lang-server-mode-list '(idris2-mode . "idris2-lsp"))
(add-to-list 'lsp-bridge-default-mode-hooks 'idris2-mode-hook)
(global-lsp-bridge-mode)
(require 'blink-search)

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
(require 'init-completion)
(require 'init-meow)

;; frameworks
(require 'init-persp)
;; languages
(require 'init-lisp)
(require 'init-rust)
(require 'init-coq)
(require 'init-python)
(require 'init-go)
(require 'init-idris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
