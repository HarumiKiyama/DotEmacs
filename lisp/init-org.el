;;; init-org.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package org-pomodoro
  :commands org-pomodoro
  :after org)

(use-package denote
  :custom
  (denote-directory (expand-file-name "~/org-mode/notes")))

(use-package ox-hugo
  :after ox
  :config
  ;; https://emacs-china.org/t/ox-hugo-auto-fill-mode-markdown/9547/4
  (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))


(use-package org-journal
  :pin nongnu
  :bind
  (:map org-journal-mode-map
        ("C-c C-c" . kill-buffer-and-window))
  :hook (org-journal-mode . auto-fill-mode)
  :config
  (setq org-journal-date-format "%Y-%m-%d %A"
        org-journal-time-format ""
        org-journal-time-prefix ""
        org-journal-dir (expand-file-name "~/org-mode/journal")))

(use-package org-super-agenda
  :after org
  :init
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (keymap-set org-super-agenda-header-map "q" 'org-agenda-quit)
  (add-hook 'org-agenda-mode-hook
            'org-super-agenda-mode))

(use-package org
  :pin gnu
  :config
  (progn
    (setq
     org-directory "~/org-mode"
     org-startup-indented t
     org-use-speed-commands t
     org-hide-emphasis-markers t
     org-startup-with-inline-images t
     org-image-actual-width '(300)
     org-agenda-dir "~/org-mode"
     deft-dir "~/org-mode"
     org-todo-keywords '((sequence "TODO(t)" "STARTED" "SUSPEND(p)" "|" "DONE(d!)" "ABORT(a!)"))
     org-todo-keyword-faces '(("STARTED" . (:inherit (bold org-scheduled-today)))
                              ("SUSPEND" . (:inherit (bold warning)))
                              ("ABORT" . (:inherit (bold error))))
     org-clock-in-switch-to-state "STARTED"
     org-clock-persist t
     org-tag-alist '(("Routine" . ?r)
                     ("Programming" . ?p)
                     ("Reading" . ?R))
     org-capture-templates '(("e" "Emacs" entry (file+headline "task.org" "Emacs Hacking") "** TODO %?")
                             ("t" "Trivial" entry (file+headline "task.org" "Trivial") "** TODO %?")
                             ("b" "Blog" entry (file "blog.org") "* SUSPEND %?")
                             ("c" "Company" entry (file+headline "task.org" "tenbeger") "*** TODO %?"))
     org-agenda-files '("~/org-mode/task.org"
                        "~/org-mode/notation.org"
                        "~/org-mode/blog.org")
     org-refile-targets '(("~/org-mode/task.org" :maxlevel . 1)
                          ("~/org-mode/notes.org" :maxlevel . 1)
                          ("~/org-mode/someday.org" :maxlevel. 1)
                          ("~/org-mode/blog.org" :maxlevel . 1)
                          (nil . (:maxlevel . 2)))
     org-refile-use-outline-path 'file
     org-archive-location "~/org-mode/archive.org::"
     org-startup-truncated nil)
    ;; org babel config
    (setq org-babel-append-languages '(("plantuml" . plantuml)
                                       ("python" . python)))
    (dolist (thing org-babel-append-languages)
      (add-to-list 'org-src-lang-modes thing)
      (org-babel-do-load-languages 'org-babel-load-languages
                                   (append org-babel-load-languages
                                           (list (cons (cdr thing) t)))))
    (setq org-plantuml-exec-mode 'plantuml)


    (require 'org-tempo)
    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    (setq org-agenda-log-mode-items '(clock closed state))
    (setq org-complete-tags-always-offer-all-agenda-tags t)
    (require 'org-compat)
    (require 'org)
    (require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq org-agenda-start-day "+0d")
    ;; Change task state to STARTED when clocking in
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

    (setq org-tags-match-list-sublevels nil)

    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
    ;; and you need install texlive-xetex on different platforms
    ;; To install texlive-xetex:
    ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
    ;; }}
    (setq org-latex-default-class "ctexart")
    (setq org-latex-pdf-process
          '(
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "rm -fr %b.out %b.log %b.tex auto"))

    (setq org-latex-listings t)

    ;;reset subtask
    (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))
    (setq org-return-follows-link t)

    (require 'org-protocol)
    ;; https://chenzaichun.github.io/post/2021-10-04-org-roam-research-start/
    ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-doom
    (require 'ox-md nil t)
    ;; C-n for the next org agenda item
    (keymap-set org-agenda-mode-map "C-p" 'org-agenda-previous-item)


    (with-eval-after-load 'org-agenda
      (keymap-set org-agenda-mode-map "P" 'org-pomodoro)
      ;; 默认显示节假日
      (setq org-agenda-include-diary t))
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ))



(use-package org-contrib
  :pin nongnu
  :init
  :after org
  (require 'org-checklist))


(provide 'init-org)
