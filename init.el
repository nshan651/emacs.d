(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;; Always start with *scratch*
;;(setq initial-buffer-choice t)

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp"))

;;;; Packages

(require 'package)

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("melpa" . 1)))

(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1))

(use-package savehist
  :init (savehist-mode t))

(use-package repeat
  :defer 10
  :init
  (repeat-mode +1))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode 1))

(load (locate-user-emacs-file
       "lisp/evil-config.el"))

(load (locate-user-emacs-file
       "lisp/theme-config.el"))

(load (locate-user-emacs-file
       "lisp/ui-config.el"))

(load (locate-user-emacs-file
       "lisp/org-config.el"))

(load (locate-user-emacs-file
       "lisp/org-roam-config.el"))

(load (locate-user-emacs-file
       "lisp/development-config.el"))

(load (locate-user-emacs-file
       "lisp/programming-languages-config.el"))

(load (locate-user-emacs-file
       "lisp/terminal-config.el"))

(load (locate-user-emacs-file
       "lisp/dired-config.el"))
