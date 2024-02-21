;; modus-themes
    (use-package modus-themes
      :ensure t
      :config
      ;; Add all your customizations prior to loading the themes
      ;;(setq modus-themes-italic-constructs t
      ;;      modus-themes-bold-constructs nil)

      ;; Maybe define some palette overrides, such as by using our presets
      ;(setq modus-themes-common-palette-overrides
      ;      modus-themes-preset-overrides-intense)

      ;; Load the theme of your choice.
      ;; (load-theme 'modus-vivendi)

      ;; (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
     )

    ;; ef-themes
    (use-package ef-themes
      :ensure t)

    ;; Doom Themes
    (use-package doom-themes
      :ensure t)

    (defun ns/toggle-theme (light-mode dark-mode)
        "Toggle between light and dark mode variants."
        ;; (interactive)
        (if (eq dark-mode (car custom-enabled-themes))
            (load-theme light-mode)
          (load-theme dark-mode)))

  ;; Set theme colors here
  (defvar ns/lightmode 'ef-duo-light)
  (defvar ns/darkmode 'ef-duo-dark)

(load-theme ns/lightmode)
(ns/toggle-theme ns/lightmode ns/darkmode)
  (define-key global-map (kbd "<f5>") (lambda () (interactive)
                                        (ns/toggle-theme ns/lightmode ns/darkmode)))

;; Set base font sizes
(defvar efs/default-font-size 110)
(defvar efs/default-variable-font-size 110)

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;; Nerd icon fonts
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; (setq-default tab-width 2)
;; (setq-default evil-shift-width tab-width)
