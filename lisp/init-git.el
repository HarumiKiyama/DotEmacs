;;; init-git.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package magit
  :commands (magit-status)
  :hook (magit-diff-visit-file . (lambda () (when smerge-mode  (hydra-smerge/body))))
  :init
  (setq magit-refs-show-commit-count 'all
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))


(use-package smerge-mode
  :ensure nil
  :pretty-hydra
  (hydra-smerge (:color pink :quit-key "q" :pre (smerge-mode 1)
                        :post (smerge-auto-leave) :hint nil)
                ("Move"
                 (("n" smerge-next "next")
                  ("p" smerge-prev "prev"))

                 "Keep"
                 (("b" smerge-keep-base "base")
                  ("u" smerge-keep-upper "upper")
                  ("l" smerge-keep-lower "lower")
                  ("a" smerge-keep-all "all")
                  ("RET" smerge-keep-current "current"))

                 "Diff"
                 (("<" smerge-diff-base-upper "upper/base")
                  ("=" smerge-diff-upper-lower "upper/lower")
                  (">" smerge-diff-base-lower "base/lower")
                  ("R" smerge-refine "refine"))

                 "Other"
                 (("C" smerge-combine-with-next "Combine")
                  ("r" smerge-resolve "resolve")
                  ("k" smerge-kill-current "kill current")))))


(use-package git-timemachine)

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(provide 'init-git)


