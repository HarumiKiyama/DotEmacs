;;; init-package.el -*- lexical-binding: t no-byte-compile: t -*-

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))

(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)


;; Set ELPA packages
(set-package-archives harumi-package-archives nil nil t)

;; Initialize packages
(package-initialize)
(setq package-enable-at-startup nil)          ; To prevent initializing twice

(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)


(setq use-package-always-ensure t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)


;; Required by `use-package'
(use-package bind-key)


(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
