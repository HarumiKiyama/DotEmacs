
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Personal information
;; (setq user-full-name "Harumi Kiyama"
;;       user-mail-address "lucius0720@hotmail.com")

;; Company email info
(setq user-full-name "王力超"
      user-mail-address "wanglc@tbamc.com")



(setq visible-bell nil
      ring-bell-function 'ignore)


(setq ns-function-modifier 'hyper)

(when sys/macp
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        command-line-ns-option-alist nil))

(when sys/linuxp
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime"))

;; optimization
;; read more at a time (was 4k)
(unless sys/linuxp
  (setq command-line-x-option-alist nil))
;; increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max (* 1024 1024))  ; 64kb
;; don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)


;; add proxy
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "localhost:7890")
     ("https" . "localhost:7890")))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-side-window-location 'bottom))

(use-package dash)


;;TODO replace with transient
(use-package major-mode-hydra)


(when (display-graphic-p)
  (use-package server
    :hook (after-init . server-mode)))

;; add compilation-mode ansi color
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; encoding
;; utf-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; history
(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (expand-file-name ".cache/places" user-emacs-directory)))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "commit_editmsg\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/g?tags$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; allow commands in minibuffers
              history-length 1000
              savehist-file (expand-file-name ".cache/history" user-emacs-directory)
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode))
  :init
  (setq column-number-mode nil
        line-number-mode nil
        ;; kill-whole-line t               ; kill line including '\n'
        line-move-visual nil
        track-eol t ; keep cursor at end of lines. require line-move-visual is nil.
        show-trailing-whitespace t
        set-mark-command-repeat-pop t) ; repeating c-spc after popping mark pops it again
  ;; visualize tab, (hard) space, newline
  (setq-default show-trailing-whitespace nil) ; don't show trailing whitespace by default
  )

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :config (setq so-long-threshold 4000))

(use-package hl-todo
  :custom
  (hl-todo-include-modes '(python-mode
                           prog-mode
                           text-mode
                           emacs-lisp-mode
                           rust-mode))
  :config
  (global-hl-todo-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))


(use-package quickrun)

;; misc
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; permanently indent with spaces, never with tabs

(setq inhibit-compacting-font-caches t  ; don’t compact font caches during gc.
      delete-by-moving-to-trash t       ; deleting files go to os's trash folder
      make-backup-files nil             ; forbide to make backup files
      auto-save-default nil             ; disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(setq-default split-height-threshold nil)
(setq-default split-width-threshold (* 2 (window-width)))
(setq recenter-positions '(top middle bottom)
      wdired-allow-to-change-permissions t)
(setq create-lockfiles nil)
(show-paren-mode t)

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(provide 'init-basic)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
