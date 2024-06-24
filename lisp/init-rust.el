;;; init-rust.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package cargo
  :hook ((rust-ts-mode . cargo-minor-mode))
  :config
  (setq cargo-process--enable-rust-backtrace t))

(use-package cargo-mode)


(major-mode-hydra-define rust-ts-mode
  (:hint nil :color blue :quit-key "q")
  ("format"
   (("b" rust-format-buffer "format"))))

(use-package rust-playground
  :defer t
  :commands (rust-playground-get-snippet-basedir)
  :config
  (setq rust-playground-basedir "~/workspace/rust/playground"))


(provide 'init-rust)
