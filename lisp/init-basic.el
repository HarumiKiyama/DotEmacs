
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Personal information
(setq user-full-name "Harumi Kiyama"
      user-mail-address "lucius0720@hotmail.com")

(setq visible-bell nil
      ring-bell-function 'ignore)

(with-no-warnings
  ;; Key Modifiers
  (setq ns-function-modifier 'hyper)
  (cond
   (sys/mac-port-p
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)))
  ;; Optimization
  ;; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))
  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max (* 1024 1024)) ; 64kb
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  ;; Garbage Collector Magic Hack
  (use-package gcmh
    :diminish
    :init
    (setq gcmh-idle-delay 5
	      gcmh-high-cons-threshold 16777216) ; 16MB
    (gcmh-mode 1)))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-side-window-location 'bottom))

(use-package server
  :ensure nil
  :hook (after-init . (lambda () (server-mode))))



;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(set-language-environment 'chinese-gbk)
(prefer-coding-system 'utf-8-auto)
(set-file-name-coding-system 'utf-8)

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (expand-file-name ".cache/places" user-emacs-directory)))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
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
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode nil
        line-number-mode nil
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  ;; Prettify the process list
  )

(use-package time
  :ensure nil
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 4000))


;; Misc
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(setq-default split-height-threshold nil)
(setq-default split-width-threshold (* 2 (window-width)))
(setq recenter-positions '(top middle bottom)
      wdired-allow-to-change-permissions t)
(setq project-find-functions '(my/project-try-local project-try-vc))
(setq create-lockfiles nil)
(show-paren-mode t)
(provide 'init-basic)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
