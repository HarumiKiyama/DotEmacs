;;; init-tools.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point+)
  :init
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+))

(use-package sudo-edit)

(use-package calibredb
  :config
  (setq calibredb-root-dir "~/Calibre Library/"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

(use-package pdf-tools)


(use-package elfeed-org
  :defer t
  :config
  (setq rmh-elfeed-org-files (list "~/org-mode/elfeed.org"))
  (elfeed-org))


(use-package org-msg
  :after gnus
  :config
  (setq mail-user-agent 'gnus-user-agent)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi%s,\n\n"
        org-msg-recipient-names '(("jeremy.compostella@gmail.com" . "Jérémy"))
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new text html)
                                       (reply-to-html text html)
                                       (reply-to-text text))
        org-msg-convert-citation t
        org-msg-signature "
 Regards,
 #+begin_signature
 --
 *harumi*
 #+end_signature")
  (org-msg-mode))

(use-package org-contacts)


(use-package gnus
  :ensure nil
  :config
  ;; Send email through SMTP
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.exmail.qq.com"
        smtpmail-smtp-service 587
        smtpmail-local-domain "compypc")

  (setq gnus-select-method '(nntp "news.gwene.org")) ;; Read feeds/atom through gwene

  ;; @see http://gnus.org/manual/gnus_397.html
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "tbamc"
                        (nnimap-address "imap.exmail.qq.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl)
                        (nnmail-expiry-wait 90)))

  ;; OPTIONAL, the setup for Microsoft Hotmail
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "hotmail"
                        (nnimap-address "outlook.office365.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl)
                        (nnmail-expiry-wait 90)))
  
  (setq gnus-parameters
        '(("company"
           (posting-style
            (address "wanglc@tbamc.com")
            ("X-Message-SMTP-Method" "smtp smtp.exmail.qq.com 465")))
          ("personal"
           (posting-style
            (address "lucius0720@hotmail.com")
            ("X-Message-SMTP-Method" "smtp smtp-mail.outlook.com 587"))))))


(provide 'init-tools)
