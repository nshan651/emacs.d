

(defun ns/prog-mode-setup ()
  "Setup some sensible defaults."
  (setq truncate-lines t) ; Avoid code wrap.
  (display-fill-column-indicator-mode 1) ; Show fill column (already set to 80).
  (flymake-mode 1) ; Basic syntax checking.
  )

(use-package eglot
  :hook
  (prog-mode . ns/prog-mode-setup)
  (eglot-managed-mode . eglot-inlay-hints-mode)
  (eglot-managed-mode . (lambda ()
                          (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
  ((c-mode
    c++-mode
    csharp-mode
    python-mode
    rust-mode
    yaml-mode
    ) . eglot-ensure)
  :config

  ;; Optional: improve eldoc display
  (setq eldoc-echo-area-use-multiline-p t)

  (setq eglot-autoshutdown t                ; Shutdown servers when last buffer closes
        eglot-sync-connect nil              ; Connect asynchronously
        eglot-events-buffer-size 0          ; Disable noisy *eglot-events* logging
        eglot-ignored-server-capabilities
        '(:documentHighlightProvider        ; Don’t auto-highlight symbol under point
          :documentOnTypeFormattingProvider ; Avoid intrusive on-type formatting
          :inlayHintProvider))
  ;; Use header line for breadcrumbs (like VSCode symbol path)
  (setq eglot-display-context 'header-line)
  )

;; TODO: Convert these keybinds to eglot.
;; (ns/leader-m 'lsp-mode-map
;;  "a" 'lsp-execute-code-action
;;  "v" 'lsp-avy-lens
;;  "n" 'lsp-describe-thing-at-point
;;  ;; "i" 'lsp-goto-implementation
;;  "d" 'lsp-find-definition
;;  "D" 'lsp-find-declaration
;;  "t" 'lsp-find-type-definition
;;  "x" 'lsp-find-references
;;  "r" 'lsp-rename
;;  "R" 'lsp-restart-workspace
;;  "=" 'lsp-format-buffer
;;  "l" 'lsp-workspace-show-log)

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
         ("C-c l d" . consult-eglot-symbols)))

(use-package dap-mode
  :after eglot-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

;; Setup TRAMP mode
(setq tramp-default-method "ssh")

(use-package yasnippet
  :disabled t
  :general
  (ns/leader-ct 'override
    "Y" #'yas-new-snippet)
  :config
  ;; Don't expand snippets in normal mode
  (general-def 'normal yas-minor-mode-map
    [remap yas-expand] #'ignore)
  (general-def input-decode-map "C-i" [C-i])
  (general-def 'insert yas-minor-mode-map
    "<C-i>" #'yas-expand))

(use-package yasnippet-snippets
  :disabled t
  :after yasnippet
  :demand t
  :config
  ;; Necessary for my personal snippets to override some of these
(yas-reload-all))

(use-package consult-yasnippet
  :disabled t
  :general ('insert "C-<tab>" #'consult-yasnippet))

(use-package project
  :bind (:map project-prefix-map
              ("t" . eat-project))
  :custom
  (project-switch-use-entire-map t))

(setq project-compilation-buffer-name-function
      '(format "*compilation: %s*" (project-name (project-current))))

(ns/leader-t 'override
  "f" '(project-find-file :wk "project find file")
  "s" '(project-switch-project :wk "project switch project")
  "g" '(consult-ripgrep :wk "consult ripgrep")
  "c" '(project-compile :wk "project compile project")
  "r" '(project-recompile :wk "project compile project")
  "d" '(project-dired :wk "project dired")
  "e" '(eat-project :wk "project eat shell"))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(ns/leader-m 'override
  "s" '(magit-status :wk "git status")
  "d" '(magit-diff :wk "git diff")
  "l " '(magit-log :wk "git log")
  "p " '(magit-push :wk "git push")
  "f " '(magit-pull  :wk "git pull")
  "c " '(magit-commit :wk "git commit")
  "a " '(magit-stage :wk "git add")
  "U " '(magit-unstage-all :wk "git reset")
  "i " '(magit-init :wk "git init"))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after 'magit)

;; (defun ns/flymake-cc-default ()
;;   "Default compile command for C++ files without a Makefile."
;;   (if buffer-file-name
;;       (let (
;;             (base-name  (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
;;         (concat "g++ --std=c++23 -g -o "
;;                 base-name
;;                 " " base-name ".cpp"
;;                 " && ./" base-name))
;;         (message "Buffer is not visiting a file")))

;; (use-package flymake
;;   :hook (c++-mode . flymake-mode)
;;   :custom
;;   ;; Automatically continue without needing to hit Enter
;;   (compilation-read-command nil)
;;   :config
;;   (setq-default compile-command '(ns/flymake-cc-default))
;;   )

;; (general-def '(normal insert visual)
;;   :keymaps 'c++-mode-map
;;   "C-c C-c" 'compile)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))

(use-package smartparens
  :init
  (smartparens-global-mode)
  :config
  ;; load default config
  (require 'smartparens-config))
