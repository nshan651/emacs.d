;; modus-themes
(use-package modus-themes)

;; ef-themes
(use-package ef-themes)

;; Doom Themes
(use-package doom-themes)

;; Set theme colors
(defvar ns/default-theme 'doom-one)
;; (defvar ns/alt-theme nil)
(defvar ns/alt-theme 'leuven)

;; Load default theme
;; (when ns/default-theme
;;   (load-theme ns/default-theme))

(defun ns/toggle-theme (default-theme alt-theme)
  "Toggle between light and dark mode variants."
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
    ;; Disable current theme to remove vestigial highlights.
    (disable-theme current-theme)
    (if (eq default-theme current-theme)
        (load-theme alt-theme)
      (load-theme default-theme))))

;; Binding to toggle between default and alt theme, inspired by 'modus-themes-toggle`
(define-key global-map (kbd "<f5>") (lambda () (interactive)
                                      (ns/toggle-theme ns/default-theme  ns/alt-theme)))

(load-theme 'modus-vivendi)
(set-face-attribute 'default nil :background "#1f1f1f")
(with-eval-after-load 'org
  (set-face-attribute 'org-block nil :background "#2b2b2b")
  (set-face-attribute 'org-verbatim nil :foreground "#fffaaa" :weight 'bold)
  (set-face-attribute 'org-code nil :foreground ";; #ff599c"))

(with-eval-after-load 'gptel
  (set-face-attribute 'gptel-rewrite-highlight-face nil
                      :background "#413847"))

(defun ns/toggle-transparency ()
  "Toggle transparency of Emacs frame."
  (interactive)
  (if (equal (car (frame-parameter (selected-frame) 'alpha)) 100)
      (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
    (set-frame-parameter (selected-frame) 'alpha '(100 . 100))))

;; Binding to toggle between default and alt theme, inspired by 'modus-themes-toggle`
(define-key global-map (kbd "<f4>") (lambda () (interactive)
                                      (ns/toggle-transparency)))

;; Set base font sizes
(defvar efs/default-font-size 110)
(defvar efs/default-variable-font-size 110)

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height
                    efs/default-variable-font-size :weight 'regular)

;; Nerd icon fonts
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Add some additional UI elements
(display-time-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
