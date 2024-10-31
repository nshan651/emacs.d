(use-package undo-fu
  :general
  ('normal
   "u" #'undo-fu-only-undo
   "U" #'undo-fu-only-redo
   "C-r" #'undo-fu-only-redo))

;; ignores encrypted files by default
(use-package undo-fu-session
  :init (undo-fu-session-global-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(defun ns/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (delete-backward-char arg)))

  ;; Enable vertico
  (use-package vertico
    :init
    (vertico-mode)
    :bind (:map minibuffer-local-map
                ("<backspace>" . ns/minibuffer-backward-kill)))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
         ;; TODO generalize (hehe, see what I did there) these keybinds later
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

;; Buffer management
(ns/leader-spc
  ;; repeat-complex-command
  "M-:" 'consult-complex-command
  "b"   '(:ignore t :wk "consult buffer selection")
  ;; switch-to-buffer
  "bb"  '(consult-buffer :wk "consult buffer")
  ;; switch-to-buffer-other-window
  "b/"  '(consult-buffer-other-window :wk "open buffer in another window")
  ;; switch-to-buffer-other-frame
  "b\\" '(consult-buffer-other-frame :wk "open buffer in another frame")
  ;; bookmark-jump
  "br"  '(consult-bookmark :wk "bookmark jump")
  ;; project-switch-to-buffer
  "bp"  '(consult-project-buffer :wk "switch between project buffers"))

;; C-c bindings in `mode-specific-map'
(general-def 'normal
  :keymaps 'mode-specific-map
  :prefix "C-c"
  "M-x" 'consult-mode-command
  "h"   'consult-history
  "k"   'consult-kmacro
  "m"   'consult-man
  "i"   'consult-info)

;; M-g bindings in `goto-map'
(general-def 'normal
  :keymaps 'goto-map
  :prefix "M-g"
  "f"   'consult-flymake         ;; Alternative: consult-flycheck
  "g"   'consult-goto-line       ;; orig. goto-line
  "M-g" 'consult-goto-line       ;; orig. goto-line
  "o"   'consult-outline         ;; Alternative: consult-org-heading
  "m"   'consult-mark
  "k"   'consult-global-mark
  "i"   'consult-imenu
  "I"   'consult-imenu-multi)

;; M-s bindings in `search-map'
(general-def '(normal insert visual emacs)
  :keymaps 'search-map
  :prefix "M-s"
  "d"   'consult-find            ;; Alt: consult-fd
  "D"   'consult-locate
  "g"   'consult-grep
  "G"   'consult-git-grep
  "r"   'consult-ripgrep
  "k"   'consult-keep-lines
  "u"   'consult-focus-lines)

;; C-s bindings in `search-mode-map'
;; Prefer `consult-line' over Isearch for a swiper-like experience
 (general-def 'override
   :keymaps 'isearch-mode-map
   "C-s" 'consult-line)

;; Other custom bindings
(general-def 'override "M-y" 'consult-yank-pop) ;; orig. yank-pop

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(general-def 'override
  "C-."   'embark-act
  "C-;"   'embark-dwim
  "C-h B" 'embark-bindings)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :demand t
  :config
  (marginalia-mode))

(use-package corfu
  ;; :general
  ;; (general-def 'corfu-mode-map
  ;;   "C-j" 'corfu-next
  ;;   "C-k" 'corfu-previous
  ;;   "TAB" 'corfu-insert
  ;;   "C-f" 'corfu-insert)
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode))

(use-package emacs
  :init
  (setq tab-always-indent 'complete))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
