;;; init-tools.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point+)
  :init
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+))

(use-package uuidgen
  :commands (uuidgen))

(use-package sudo-edit)

(use-package calibredb
  :config
  (setq calibredb-root-dir "~/Calibre Library/"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

(use-package pdf-tools)

(provide 'init-tools)
