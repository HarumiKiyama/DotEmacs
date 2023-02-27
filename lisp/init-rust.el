;;; init-rust.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "RET") 'av/auto-indent-method-maybe))


(use-package cargo
  :hook ((rust-mode . cargo-minor-mode))
  :config
  (defun my/cargo-test-current ()
    (interactive)
    (setenv "RUST_LOG" "debug")
    (cargo-process-current-test))
  :bind (:map rust-mode-map
              (("C-c C-t" . my/cargo-test-current)))
  :custom ((cargo-process--command-current-test "test --color never")
           (cargo-process--enable-rust-backtrace t)
           (cargo-process--command-flags "--  --nocapture")))

(use-package rust-playground
  :defer t
  :custom (rust-playground-run-command "cargo run --color never")
  :commands (rust-playground-get-snippet-basedir)
  :config
  (setq rust-playground-basedir (expand-file-name "~/workspace/rust/playground")))


(provide 'init-rust)
