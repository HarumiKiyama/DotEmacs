;;; init-lisp.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (lispy-define-key lispy-mode-map "e" 'eval-last-sexp))

(provide 'init-lisp)
