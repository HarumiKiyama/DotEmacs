;;;;  -*- lexical-binding: t; -*-
(require 'init-funcs)

(use-package all-the-icons
  :ensure t)

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-'") 'vertico-quick-jump)
  (define-key vertico-map [backspace] #'vertico-directory-delete-char))

(use-package consult
  :ensure t
  :defer t
  :init
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'consult-line
              :around
              #'harumi/consult-line
              '((name . "wrapper")))
  :config
  (setq ;; consult-project-root-function #'doom-project-root
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay 0.15
   consult-async-input-throttle 0.2
   consult-async-input-debounce 0.1))

(use-package orderless
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((equal "!" word) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 1))
        (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :init
  :config)

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq which-key-use-C-h-commands nil
	    ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
	    prefix-help-command #'embark-prefix-help-command)
  (setq
   embark-verbose-indicator-display-action
   '((display-buffer-at-bottom)
     (window-parameters (mode-line-format . none))
     (window-height . fit-window-to-buffer)))
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))



(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)   ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))
(setq acm-candidate-match-function 'orderless-flex)


(provide 'init-completion)
