;; -*- lexical-binding: t; -*-
(use-package xterm-color
  :config
  ;; Enable ANSI escape handling in eshell
  (with-eval-after-load 'eshell
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (setenv "TERM" "xterm-256color"))

  ;; Enable ANSI escape handling in M-x compile
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun ns/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'ns/advice-compilation-filter))

(use-package eat
  :bind (("C-c e" . eat))
  :custom
  (eat-term-name "xterm-256color")
  (explicit-shell-file-name "/bin/zsh")
  (shell-file-name "/bin/zsh")
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode))
