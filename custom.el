(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-plus-contrib company yasnippet counsel evil-smartparens zenburn-theme powerline all-the-icons swiper smartparens which-key whick-key ivy evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))

;; (set-face-attribute 'default nil
;;                     :family "Dejavu sans mono"
;;                     :height 150
;;                     :weight 'normal
;;                     :width 'normal)

(setq
   backup-by-copying t  
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)
;; TODO set default font


;; orgmode setting
(with-eval-after-load 'org
    (setq org-todo-keywords '((sequence "TODO(t)" "TESTING(t)" "SUSPEND(p)" "|"
                                        "DONE(d!)" "ABORT(a)")))
    (setq org-tag-alist '(("@company" . ?C)
                          ("crypt" . ?c)
                          ("routine" . ?r)
                          ("Haskell" . ?h)
                          ("Idris" . ?i)
                          ("Python" . ?p)
                          ("Algorithms" . ?a)
                          ("reading" . ?R)
                          ("Japanese" . ?j)
                          ("English" . ?e)
                          ))
    (setq org-capture-templates '(("w" "Words" entry (file+headline "~/org-mode/Esperanto.org" "Words")
                                   "** word :drill:\n%^{Esperanto}[%^{English}]")
                                  ))
    (setq org-agenda-files '("~/org-mode/task.org"
                             "~/org-mode/notation.org"
                             "~/org-mode/routine.org"
                             "~/org-mode/Esperanto.org"))
    (setq org-refile-targets '(("~/org-mode/task.org" :maxlevel . 1)
                               ("~/org-mode/notes.org" :maxlevel . 1)
                               ("~/org-mode/someday.org" :maxlevel . 1)
                               ("~/org-mode/blog.org" :maxlevel . 1)
                               (nil . (:maxlevel . 2))
                               ))
    (setq org-archive-location "~/org-mode/archive.org::")
    (setq org-startup-truncated nil)
    ;; org-journal setting
    (setq org-journal-date-format "%Y-%m-%d %A"
          org-journal-time-format ""
          org-journal-time-prefix "")
    ;; org-crypt setting
    (setq org-crypt-key "B77016C8B8ECEBE817DC0288CC09EA1921BDC71F"
          auto-save-default nil)
    (define-key org-mode-map (kbd "\C-ct") 'my-org/diary-titles)
    (define-key org-mode-map (kbd "\C-cd") 'org-drill)
    )
