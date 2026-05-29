;; Pull in some nice themes.
(use-package modus-themes) ; Included since Emacs 27+.
(use-package ef-themes)
(use-package doom-themes)

(setq modus-themes-common-palette-overrides
      '((bg-main "#1f1f1f")
        (bg-dim "#2b2b2b")
        (prose-code "#ff599c")
        (prose-verbatim "#fffaaa")))

(load-theme 'modus-vivendi)

(defun ns/toggle-transparency ()
  "Toggle transparency of Emacs frame."
  (interactive)
  (if (equal (car (frame-parameter (selected-frame) 'alpha)) 100)
      (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
    (set-frame-parameter (selected-frame) 'alpha '(100 . 100))))

;; Binding to toggle between default and alt theme, inspired by 'modus-themes-toggle`
(define-key global-map (kbd "<f4>") (lambda () (interactive)
                                      (ns/toggle-transparency)))

;; Set base font sizes.
(defvar ns/default-font-size 140)
(defvar ns/default-fixed-font-size 130)
(defvar ns/default-variable-font-size 110)

(set-face-attribute 'default nil
                    :font "Hack"
                    :weight 'normal
                    :height ns/default-font-size)

(set-face-attribute 'fixed-pitch nil
                    :font "Hack"
                    :weight 'normal
                    :height ns/default-fixed-font-size)

(set-face-attribute 'variable-pitch nil
                    :font "Hack"
                    :weight 'normal
                    :height ns/default-variable-font-size)

;; Nerd icon fonts
(use-package nerd-icons
  :custom
  ;; "Symbols Nerd Font Mono" is the recommended default.
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Add some additional UI elements
(display-time-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
