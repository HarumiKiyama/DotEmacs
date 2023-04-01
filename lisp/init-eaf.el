(use-package eaf
  :straight (eaf
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*")
             :includes (eaf-pdf-viewer eaf-browser)
             ;; :post-build (("python" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps"))
             ))


(use-package eaf-browser
  :after (eaf)
  :ensure nil)

(use-package eaf-pdf-viewer
  :after (eaf)
  :ensure nil)

(provide 'init-eaf)
