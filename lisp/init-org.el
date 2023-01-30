;;; init-org.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package org-pomodoro
  :ensure t
  :commands org-pomodoro
  :after org)


(use-package org-super-agenda
  :after org
  :init
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (define-key org-super-agenda-header-map (kbd "q") 'org-agenda-quit)
  (setq org-super-agenda-groups
        '((:name "Important"
                 :priority "A")
          (:name "Quick Picks"
                 :effort< "0:30")
          (:name "Next Items"
                 :tag ("NEXT" "outbox"))
          (:priority<= "B"
                       :scheduled future)))
  (add-hook 'org-agenda-mode-hook
            'org-super-agenda-mode))


(use-package org
  :ensure t
  :pin gnu)

(with-eval-after-load 'org
  (progn
    (setq org-directory "~/org-mode")
    (setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300)
          org-agenda-dir "~/org-mode"
          deft-dir "~/org-mode"
          org-todo-keywords '((sequence "TODO(t)" "START" "SUSPEND(p)" "|" "DONE(d!)" "ABORT(a!)"))
          org-todo-keyword-faces '(("START" . (:inherit (bold org-scheduled-today)))
                                   ("SUSPEND" . (:inherit (bold warning)))
                                   ("ABORT" . (:inherit (bold error))))
          org-clock-in-switch-to-state "START"
          org-clock-out-switch-to-state "TODO"
          org-clock-persist t
          org-tag-alist '(("Routine" . ?r)
                          ("Programming" . ?p)
                          ("Reading" . ?R))
          org-capture-templates '(("e" "Emacs" entry (file+headline "task.org" "Emacs Hacking") "** TODO %?")
                                  ("a" "Algorithm" entry (file +create-algorithm-org-file) "* Description\n%?\n* Solution")
                                  ("t" "Trivial" entry (file+headline "task.org" "Trivial") "** TODO %?")
                                  ("b" "Blog" entry (file "blog.org") "* SUSPEND %?"))
          org-agenda-files '("~/org-mode/task.org"
                             "~/org-mode/notation.org"
                             "~/org-mode/blog.org")
          org-refile-targets '(("~/org-mode/task.org" :maxlevel . 1)
                               ("~/org-mode/notes.org" :maxlevel . 1)
                               ("~/org-mode/someday.org" :maxlevel . 1)
                               ("~/org-mode/blog.org" :maxlevel . 1)
                               (nil . (:maxlevel . 2)))
          org-refile-use-outline-path 'file
          org-archive-location "~/org-mode/archive.org::"
          org-startup-truncated nil)


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
        (ad-set-arg 1 fixed-contents)))

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
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

    (setq org-tags-match-list-sublevels nil)

    (add-hook 'org-mode-hook (lambda ()
                               ;; keybinding for editing source code blocks
                               (when (featurep 'company)
                                 (company-mode -1))
                               ;; keybinding for inserting code blocks
                               (local-set-key (kbd "C-c i s")
                                              'harumi/org-insert-src-block)))
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

    ;; (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
    (setq org-return-follows-link t)

    (setq org-plantuml-jar-path
          (expand-file-name "~/.doom.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.doom.d/ditaa.jar")

    (require 'org-protocol)
    ;; https://chenzaichun.github.io/post/2021-10-04-org-roam-research-start/


    ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#configuring-doom
    ;; (org-babel-do-load-languages
    ;;  'org-babel-load-languages
    ;;  '((perl . t)
    ;;    (ruby . t)
    ;;    (shell . t)
    ;;    (dot . t)
    ;;    (typescript . t)
    ;;    (js . t)
    ;;    (latex .t)
    ;;    (python . t)
    ;;    (emacs-lisp . t)
    ;;    (plantuml . t)
    ;;    (C . t)
    ;;    (ditaa . t)))

    (progn

      (use-package cal-china-x
        :ensure t
        :demand t)
      (setq mark-holidays-in-calendar t)
      (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
      (setq calendar-holidays
            (append cal-china-x-important-holidays
                    cal-china-x-general-holidays)))
    (require 'ox-md nil t)

    ;; define the refile targets

    ;; C-n for the next org agenda item
    (define-key org-agenda-mode-map (kbd "C-p") 'org-agenda-previous-item)


    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
      ;; 默认显示节假日
      (setq org-agenda-include-diary t))
    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal

    (with-eval-after-load 'org-capture
      (defun org-hugo-new-subtree-post-capture-template ()
        "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
        (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
               (fname (org-hugo-slug title)))
          (mapconcat #'identity
                     `(
                       ,(concat "* TODO " title)
                       ":PROPERTIES:"
                       ,(concat ":EXPORT_FILE_NAME: " fname)
                       ":END:"
                       "\n\n")          ;Place the cursor here finally
                     "\n")))

      (add-to-list 'org-capture-templates
                   '("h"                ;`org-capture' binding + h
                     "Hugo post"
                     entry
                     ;; It is assumed that below file is present in `org-directory'
                     ;; and that it has a "Blog Ideas" heading. It can even be a
                     ;; symlink pointing to the actual location of all-posts.org!
                     (file+headline org-agenda-file-blogposts "Blog Ideas")
                     (function org-hugo-new-subtree-post-capture-template))))


    (add-to-list 'org-agenda-custom-commands
                 '("r" "Daily Agenda Review"
                   ((agenda "" ((org-agenda-overriding-header "今日记录")
                                (org-agenda-span 'day)
                                (org-agenda-show-log 'clockcheck)
                                (org-agenda-start-with-log-mode nil)
                                (org-agenda-log-mode-items '(closed clock state))
                                (org-agenda-clockreport-mode t))))))

    ;; hack for org headline toc
    (defun org-html-headline (headline contents info)
      "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
      (unless (org-element-property :footnote-section-p headline)
        (let* ((numberedp (org-export-numbered-headline-p headline info))
               (numbers (org-export-get-headline-number headline info))
               (section-number (and numbers
                                    (mapconcat #'number-to-string numbers "-")))
               (level (+ (org-export-get-relative-level headline info)
                         (1- (plist-get info :html-toplevel-hlevel))))
               (todo (and (plist-get info :with-todo-keywords)
                          (let ((todo (org-element-property :todo-keyword headline)))
                            (and todo (org-export-data todo info)))))
               (todo-type (and todo (org-element-property :todo-type headline)))
               (priority (and (plist-get info :with-priority)
                              (org-element-property :priority headline)))
               (text (org-export-data (org-element-property :title headline) info))
               (tags (and (plist-get info :with-tags)
                          (org-export-get-tags headline info)))
               (full-text (funcall (plist-get info :html-format-headline-function)
                                   todo todo-type priority text tags info))
               (contents (or contents ""))
               (ids (delq nil
                          (list (org-element-property :CUSTOM_ID headline)
                                (org-export-get-reference headline info)
                                (org-element-property :ID headline))))
               (preferred-id (car ids))
               (extra-ids
                (mapconcat
                 (lambda (id)
                   (org-html--anchor
                    (if (org-uuidgen-p id) (concat "ID-" id) id)
                    nil nil info))
                 (cdr ids) "")))
          (if (org-export-low-level-p headline info)
              ;; This is a deep sub-tree: export it as a list item.
              (let* ((type (if numberedp 'ordered 'unordered))
                     (itemized-body
                      (org-html-format-list-item
                       contents type nil info nil
                       (concat (org-html--anchor preferred-id nil nil info)
                               extra-ids
                               full-text))))
                (concat (and (org-export-first-sibling-p headline info)
                             (org-html-begin-plain-list type))
                        itemized-body
                        (and (org-export-last-sibling-p headline info)
                             (org-html-end-plain-list type))))
            (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                  (first-content (car (org-element-contents headline))))
              ;; Standard headline.  Export it as a section.
              (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                      (org-html--container headline info)
                      (org-export-get-reference headline info)
                      (concat (format "outline-%d" level)
                              (and extra-class " ")
                              extra-class)
                      (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                              level
                              preferred-id
                              extra-ids
                              (concat
                               (and numberedp
                                    (format
                                     "<span class=\"section-number-%d\">%s</span> "
                                     level
                                     (mapconcat #'number-to-string numbers ".")))
                               full-text)
                              level)
                      ;; When there is no section, pretend there is an
                      ;; empty one to get the correct <div
                      ;; class="outline-...> which is needed by
                      ;; `org-info.js'.
                      (if (eq (org-element-type first-content) 'section) contents
                        (concat (org-html-section first-content "" info) contents))
                      (org-html--container headline info)))))))))


(use-package org-roam
  :init
  (defun jethro/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
    (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${type:10} ${title:20} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n E" . org-roam-extract-subtree)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package org-contrib
  :pin nongnu
  :init
  :after org
  (require 'org-checklist))

(use-package org-roam-ui
  :ensure t
  :commands (org-roam-ui-mode)
  :after org
  )

(use-package consult-org-roam
  :ensure nil
  :init
  :commands (consult-org-roam-forward-links)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "s-."))
  :bind
  ("C-c n e" . consult-org-roam-forward-links)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n s" . consult-org-roam-search))

(use-package org-download
  ;; :ensure-system-package (pngpaste . "brew install pngpaste")
  :ensure t
  :demand t
  :after org
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "./img"
                ;; org-download-screenshot-method "screencapture -i %s"
                org-download-screenshot-method "pngpaste %s"
                org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (dw/org-present-prepare-slide)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . dw/org-present-next)
         ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(provide 'init-org)
