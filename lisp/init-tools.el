;;; init-tools.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package sudo-edit)

(unless sys/linuxp
  (use-package calibredb
    :defer t
    :config
    (setq calibredb-root-dir "~/Calibre Library/"
          calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))
  (use-package pdf-tools
    :defer t)

  (use-package nov
    :defer t))


(use-package org-msg
  :after gnus
  :config

  (defun harumi-change-smtp ()
    (save-excursion
      (let* ((from
              (save-restriction
                (message-narrow-to-headers)
                (message-fetch-field "from")))
             (address
              (cadr (mail-extract-address-components from)))
             (start (string-match "@\\(.*\\)" address))
             (server (match-string 1 address)))
        (cond
         ((string-match "tbamc.com" server)
          (setq smtpmail-smtp-server "smtp.exmail.qq.com"
                smtpmail-smtp-service 465
                smtpmail-stream-type 'ssl))
         ((string-match "hotmail.com" server)
          (setq smtpmail-smtp-server "smtp-mail.outlook.com"
                smtpmail-smtp-service 587
                smtpmail-stream-type 'starttls))))))
  
  (add-hook 'message-send-hook 'harumi-change-smtp)

  (setq
   mail-user-agent 'gnus-user-agent
   message-send-mail-function 'smtpmail-send-it
   smtpmail-local-domain "compypc")

  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-recipient-names '(("lucius0720@hotmail.com" . "harumi")
                                  ("h.kiyam0720@gmail.com" . "harumi")
                                  ("wanglc@tbamc.com" . "wanglc"))
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new html)
                                       (reply-to-html html)
                                       (reply-to-text html))
        org-msg-convert-citation t
        org-msg-signature "
 #+begin_signature
 --
 Regards,
 *harumi*
 #+end_signature"
        )
  (org-msg-mode)
  (org-msg-mode-message)
  (org-msg-mode-gnus))

(use-package ebdb)


(use-package gnus
  :ensure nil
  :config
  (keymap-substitute gnus-summary-mode-map 'gnus-summary-reply 'gnus-article-wide-reply-with-original)
  (keymap-substitute gnus-article-mode-map 'gnus-summary-reply 'gnus-article-wide-reply-with-original)

  ;; config gnus summary show format
  (setq gnus-summary-line-format "%U%z %(%&user-date; %-15,15f %B%s%)\n"
        gnus-user-date-format-alist
        '((t . "%Y-%m-%dT%H:%M")))
  
  
  ;; Send email through SMTP
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
