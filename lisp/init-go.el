(use-package go-mode
  :config
  (setq gofmt-command "goimports"))

(major-mode-hydra-define go-ts-mode
  (:hint nil :color blue :quit-key "q")
  ("format"
   (("b" gofmt "format"))))


(setq go-ts-mode-indent-offset 4)


(provide 'init-go)
