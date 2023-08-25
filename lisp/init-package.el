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


;; Setup `package-vc`
(require 'cl-lib)
(require 'use-package-core)

(cl-defun slot/vc-install (&key (fetcher "github") repo name rev backend)
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (package-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p package-name)
      (package-vc-install url iname rev backend))))


(defvar package-vc-use-package-keyword :vc)

(defun package-vc-use-package-set-keyword ()
  (unless (member package-vc-use-package-keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((pos (cl-position :unless use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                 (tail (nthcdr (+ 1 pos) use-package-keywords)))
            (append head (list package-vc-use-package-keyword) tail)))))

(defun use-package-normalize/:vc (name-symbol keyword args)
  (let ((arg (car args)))
    (pcase arg
      ((or `nil `t) (list name-symbol))
      ((pred symbolp) args)
      ((pred listp) (cond
                     ((listp (car arg)) arg)
                     ((string-match "^:" (symbol-name (car arg))) (cons name-symbol arg))
                     ((symbolp (car arg)) args)))
      (_ nil))))

(defun use-package-handler/:vc (name-symbol keyword args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code is
    ;; compiled or evaluated.
    (if args
        (use-package-concat
         `((unless (package-installed-p ',(pcase (car args)
                                            ((pred symbolp) (car args))
                                            ((pred listp) (car (car args)))))
             (apply #'slot/vc-install ',(cdr args))))
         body)
      body)))

(defun package-vc-use-package-override-:ensure (func name-symbol keyword ensure rest state)
  (let ((ensure (if (plist-member rest :vc)
                    nil
                  ensure)))
    (funcall func name-symbol keyword ensure rest state)))

(defun package-vc-use-package-activate-advice ()
  (advice-add
   'use-package-handler/:ensure
   :around
   #'package-vc-use-package-override-:ensure))

(defun package-vc-use-package-deactivate-advice ()
  (advice-remove
   'use-package-handler/:ensure
   #'package-vc-use-package-override-:ensure))

;; register keyword on require
(package-vc-use-package-set-keyword)


(setq use-package-always-ensure t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)


;; Required by `use-package'
(use-package bind-key)


;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
