(use-package ccls
  :hook lsp
  (:hook-into c-mode c++-mode))

;; Load SLY
(require 'sly)

;; Set the Common Lisp implementation to SBCL
(setq inferior-lisp-program "sbcl")

(add-to-list 'sly-contribs 'sly-asdf 'append)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
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
