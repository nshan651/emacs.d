(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(set-fringe-mode 10) ; Give some breathing room

(menu-bar-mode -1) ; Disable the menu bar

;; Disable graphical elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Default for fill column should be 80 (not 70 like emacs would have it!!!)
(setq-default fill-column 80)

;; Improve mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Start scrolling when cursor is with 25% of the top or bottom of the screen.
(setq scroll-margin (floor (* 0.25 (window-height))))

;; Vim-like key scrolling
(setq scroll-step 1
      scroll-conservatively 10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Relative line numbers
(menu-bar--display-line-numbers-mode-relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar ns-emacs--file-name-handler-alist file-name-handler-alist)
(defvar ns-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
                  file-name-handler-alist ns-emacs--file-name-handler-alist
                  vc-handled-backends ns-emacs--vc-handled-backends)))

(setq custom-safe-themes t)

;; Initialise installed packages at this early stage, by using the available cache.
(setq package-enable-at-startup t)
