(defun ns/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((lsp-mode . ns/lsp-mode-setup)
   (python-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;;(ns/leader-m 'lsp-mode-map
;;  "a" 'lsp-execute-code-action
;;  "v" 'lsp-avy-lens
;;  "n" 'lsp-describe-thing-at-point
;;  "i" 'lsp-goto-implementation
 ;; "d" 'lsp-find-definition
  ;;"D" 'lsp-find-declaration
  ;;"t" 'lsp-find-type-definition
  ;;"x" 'lsp-find-references
  ;;"r" 'lsp-rename
  ;;"R" 'lsp-restart-workspace
  ;;"=" 'lsp-format-buffer
  ;;"l" 'lsp-workspace-show-log)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package consult-lsp
  :after lsp-mode
  :general
  (ns/leader-m 'lsp-mode-map
    "y" 'consult-lsp-symbols
    "e" 'consult-lsp-diagnostics))

(use-package lsp-treemacs
  :after lsp)

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  ;; use treesitter where possible
  (global-treesit-auto-mode))

(use-package dap-mode
  :disabled t
  :commands dap-debug
  :init
  :config
  (dap-ui-mode 1)
  (dap-mode 1))

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

(use-package yasnippet
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
  :after yasnippet
  :demand t
  :config
  ;; Necessary for my personal snippets to override some of these
(yas-reload-all))

(use-package consult-yasnippet
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

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after 'magit)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))
