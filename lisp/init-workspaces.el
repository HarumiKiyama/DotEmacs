;;; init-workspaces.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it.
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :after consult
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")

  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  :config
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))
    "Set workspace buffer list for consult-buffer.")
  )

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))



(provide 'init-workspaces)
