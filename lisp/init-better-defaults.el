;;;;  -*- lexical-binding: t; -*-

(require 'init-funcs)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              (("j" . find-file)))
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
  (sp-local-pair 'rust-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil))

(provide 'init-better-defaults)
