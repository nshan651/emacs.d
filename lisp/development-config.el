(defun ns/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(defun ns/prog-mode-setup ()
  (display-fill-column-indicator-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook
  ((lsp-mode . ns/lsp-mode-setup)
   (prog-mode . ns/prog-mode-setup)
   (c-mode . lsp)
   (c++-mode . lsp)
   (csharp-mode . lsp)
   (go-mode . lsp)
   (python-mode . lsp)
   (rust-mode . lsp))
  :commands lsp
  ;; :custom
  ;; (lsp-auto-configure t)
  ;; (lsp-enable-symbol-highlighting t)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-completion-enable t)
  (setq lsp-completion-provider :capf)
  ;; (setq lsp-log-io t)
  ;; The path to lsp-mode needs to be added to load-path as well as the
  ;; path to the `clients' subdirectory.

  ;; Activate lsp-mode
  )

(ns/leader-m 'lsp-mode-map
 "a" 'lsp-execute-code-action
 "v" 'lsp-avy-lens
 "n" 'lsp-describe-thing-at-point
 ;; "i" 'lsp-goto-implementation
 "d" 'lsp-find-definition
 "D" 'lsp-find-declaration
 "t" 'lsp-find-type-definition
 "x" 'lsp-find-references
 "r" 'lsp-rename
 "R" 'lsp-restart-workspace
 "=" 'lsp-format-buffer
 "l" 'lsp-workspace-show-log)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.0)
  (lsp-ui-sideline-show-code-actions nil)
  )

(use-package consult-lsp
  :after lsp-mode
  :general
  (ns/leader-m 'lsp-mode-map
    "y" 'consult-lsp-symbols
    "e" 'consult-lsp-diagnostics))

(use-package lsp-treemacs
  :after lsp)

(use-package treesit-auto
  :disabled t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'ivy))
  :custom ((projectile-completion-system 'default))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(ns/leader-m 'override
  "p"  '(:ignore p :wk "projectile commands")
  "pf" '(projectile-find-file :wk "projectile find file")
  "ps" '(projectile-switch-project :wk "projectile switch project")
  "pg" '(consult-ripgrep :wk "consult ripgrep")
  "pp" '(projectile-find-file :wk "projectile find file")
  "pc" '(projectile-compile-project :wk "projectile compile project")
  "pd" '(projectile-dired :wk "projectile dired"))

;; (use-package counsel-projectile
;;   :after projectile
;;   :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(ns/leader-m 'override
  "g"  '(:ignore g :wk "magit commands")
  "gs" '(magit-status :wk "git status")
  "gd" '(magit-diff :wk "git diff")
  "gl " '(magit-log :wk "git log")
  ;; Pushing and pulling
  "gk " '(magit-push :wk "git push")
  "gj " '(magit-pull  :wk "git pull")
  "gc " '(magit-commit :wk "git commit")
  "ga " '(magit-stage :wk "git add")
  "gi " '(magit-init :wk "git init"))

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
  ;; :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :init
  (smartparens-global-mode)
  :config
  ;; load default config
  (require 'smartparens-config))
