(use-package ccls
  :custom
  (ccls-executable "/usr/bin/ccls"))

;; Load SLY
(require 'sly)
;; Set the Common Lisp implementation to SBCL
(setq inferior-lisp-program "/usr/bin/sbcl")

(add-to-list 'sly-contribs 'sly-asdf 'append)

;; Enable history searching
(general-def '(normal insert)
   :keymaps 'sly-mode-map
   "C-c C-c" 'sly-overlay-eval-defun
   "C-t" 'isearch-backward)

(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(guile)))

(use-package geiser-guile
  :ensure t)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (lsp-pyls-server-command '("/home/nick/.local/lib/python3.10/site-packages/pyls
"))
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (setq-default python-indent-offset 4))

;; (use-package pyvenv
;;   :after python-mode
;;   :config
;;   (pyvenv-mode 1))
