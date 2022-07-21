;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2022 zilongshanren

;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/emacs.d


;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell-correct
  :ensure t
  :init)

(use-package ispell
  :ensure nil
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))

(use-package corfu-english-helper
  :ensure nil
  :commands toggle-corfu-english-helper)

(use-package olivetti
  :init
  (setq olivetti-body-width nil)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (olivetti-mode t)
      (progn
        (olivetti-mode 0))))
  :bind
  (("<f9>" . distraction-free)))


(use-package ox-hugo
  :ensure t                             ;Auto-install the package from Melpa
  :pin melpa ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :commands org-hugo-export-to-md)

(use-package pangu-spacing
  :defer t
  :init (progn (global-pangu-spacing-mode 1)

               ;; Always insert `real' space in org-mode.
               (add-hook 'org-mode-hook
                         (lambda ()
                           (setq-local pangu-spacing-real-insert-separtor t)))))

(provide 'init-writing)
