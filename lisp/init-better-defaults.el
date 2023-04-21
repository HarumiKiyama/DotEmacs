;;;;  -*- lexical-binding: t; -*-

(require 'init-funcs)

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t
        dired-listing-switches "-alh"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "open")
          ("\\.docx\\'" "open")
          ("\\.\\(?:djvu\\|eps\\)\\'" "open")
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
          ("\\.\\(?:xcf\\)\\'" "open")
          ("\\.csv\\'" "open")
          ("\\.tex\\'" "open")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
           "open")
          ("\\.\\(?:mp3\\|flac\\)\\'" "open")
          ("\\.html?\\'" "open")
          ("\\.md\\'" "open"))))

(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$")))


(use-package smartparens
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

(use-package autorevert
  :ensure nil
  :hoot (after-init . global-auto-revert-mode))

(provide 'init-better-defaults)
