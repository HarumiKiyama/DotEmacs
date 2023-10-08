(defvar server-history '())

(defun consult--server-candidates ()
  (mapcar 'car (remove nil (tramp-parse-rhosts "~/.ssh/config"))))

(defun harumi/open-server-term (server)
  "If server NAME exists, open it, otherwise create a new server session with NAME."
  (interactive
   (list
    (consult--read
     (consult--server-candidates)
     :prompt "Server: "
     ;; TODO: add show preview
     :history 'server-history
     )))
  (let ((buffer (eat-make (format "REMOTE %s" server) "/usr/bin/env" nil "ssh" server)))
    (pop-to-buffer-same-window buffer)))

(provide 'init-server)
