(use-package ccls
  ;; :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp-deferred)))
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred))
  :custom
  (ccls-executable "/usr/bin/ccls"))

;; Load SLY
(require 'sly)

;; Set the Common Lisp implementation to SBCL
(setq inferior-lisp-program "sbcl")

(add-to-list 'sly-contribs 'sly-asdf 'append)

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
    ;; :config
    ;; (require 'dap-python)
    )

;; (use-package pyvenv
;;   :after python-mode
;;   :config
;;   (pyvenv-mode 1))
