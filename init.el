(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable these.
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these.
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(project-eshell overwrite-mode iconify-frame diary))

(defun ns/load-lisp-config (file-list)
  "Loads all config files in lisp directory."
  (dolist (file file-list)
    (let ((full-path (locate-user-emacs-file (concat "lisp/" file))))
      (when (file-readable-p full-path)
        (load full-path)))))

;; Load paths in a specific order.
(let ((ns/file-list
       '("evil-config.el"
         "theme-config.el"
         "development-config.el"
         "ui-config.el"
         "org-config.el"
         "org-roam-config.el"
         "programming-languages-config.el"
         "terminal-config.el"
         "applications-config.el"
         "window-config.el"
         "dired-config.el")))

  (ns/load-lisp-config ns/file-list))

;; Load gptel system prompts.
;; (load (locate-user-emacs-file "etc/prompts.el"))

;; Also load `feed.el'.
(load (locate-user-emacs-file "etc/feed.el"))

;; Packages
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
  :defer t
  :init
  (dired-async-mode 1))

(use-package savehist
  :init
  (setq history-length 25)
  (savehist-mode t))

(use-package repeat
  :defer 10
  :init
  (repeat-mode +1))

(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

(setq select-enable-clipboard t)
(setq x-select-enable-clipboard t)

(use-package auth-source-pass
  :custom
  (auth-source-pass-filename
   (expand-file-name
    "password-store/apikeys"
    (or (getenv "XDG_DATA_HOME")
        (expand-file-name "~/.local/share"))))
  :init
  (auth-source-pass-enable))
