
(use-package erc
  :ensure nil
  :config
  (require 'erc-sasl)
  (setq erc-autojoin-channels-alist '(("Libera.Chat" "#emacs"))
        erc-kill-buffer-on-part t
        erc-nick "harumi"
        erc-user-full-name "harumi kiyama"
        user-login-name "harumi"
        erc-password "zql1225")

  (add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

  (defun erc-login ()
    "Perform user authentication at the IRC server. (PATCHED)"
    (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                     (erc-current-nick)
                     (user-login-name)
                     (or erc-system-name (system-name))
                     erc-session-server
                     erc-session-user-full-name))
    (if erc-session-password
        (erc-server-send (format "PASS %s" erc-session-password))
      (message "Logging in without password"))
    (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
      (erc-server-send "CAP REQ :sasl"))
    (erc-server-send (format "NICK %s" (erc-current-nick)))
    (erc-server-send
     (format "USER %s %s %s :%s"
             ;; hacked - S.B.
             (if erc-anonymous-login erc-email-userid (user-login-name))
             "0" "*"
             erc-session-user-full-name))
    (erc-update-mode-line))

  )




(provide 'init-irc)
