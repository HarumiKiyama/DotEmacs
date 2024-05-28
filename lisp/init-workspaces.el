;;; init-workspaces.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it.
  :load-path "site-list/tabspaces.el"
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :after consult
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t))


(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)))


(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


(provide 'init-workspaces)
