(use-package py-isort
  :commands py-isort-buffer)

(use-package blacken
  :commands blacken-buffer)


(major-mode-hydra-define python-mode (:color blue :hint nil :quit-key "q")
  ("FORMAT"
   (("b" blacken-buffer "buffer")
    ("i" py-isort-buffer "import")
    ("p" poetry "poetry"))))

(use-package jupyter)

(use-package poetry
  :config
  (defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
    (let* ((json-object-type 'plist)
           (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
           (custom-config (expand-file-name "pyright.json" custom-dir))
           (default-config (json-read-file (expand-file-name "repo/lsp-bridge/langserver/pyright.json" user-emacs-directory)))
           (settings (plist-get default-config :settings)))

      (plist-put settings :pythonPath (executable-find "python"))

      (make-directory (file-name-directory custom-config) t)

      (with-temp-file custom-config
        (insert (json-encode default-config)))

      custom-config))

  (add-hook 'python-ts-mode-hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))
  
  (advice-add 'poetry-venv-workon :after
              (lambda (&rest args)
                (lsp-bridge-restart-process)))
  
  (advice-add 'poetry-venv-deactivate :after
              (lambda (&rest args)
                (lsp-bridge-restart-process)))

  )


(provide 'init-python)
