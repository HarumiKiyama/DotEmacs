;;; init-tools.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point+)
  :init
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+))

(use-package uuidgen
  :ensure t
  :commands (uuidgen))

(use-package sudo-edit
  :ensure t)

(provide 'init-tools)
