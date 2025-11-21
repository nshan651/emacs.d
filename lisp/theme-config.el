;; Pull in some nice themes.
(use-package modus-themes) ; Included since Emacs 27+.
(use-package ef-themes)
(use-package doom-themes)

(load-theme 'modus-vivendi)

;; Lighten the background.
(set-face-attribute 'default nil :background "#1f1f1f")

;; Enhance org-mode elements.
(with-eval-after-load 'org
  (set-face-attribute 'org-block nil :background "#2b2b2b")
  (set-face-attribute 'org-verbatim nil :foreground "#fffaaa" :weight 'bold)
  (set-face-attribute 'org-code nil :foreground "#ff599c"))

;; Make gptel-rewrite more readable.
(with-eval-after-load 'gptel-rewrite
  (set-face-attribute 'gptel-rewrite-highlight-face nil
                      :inherit 'default
                      :background "#413847"
                      :extend t))

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
(defvar ns/default-font-size 110)
(defvar ns/default-variable-font-size 110)

(set-face-attribute 'default nil :font "Hack" :height ns/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack" :height ns/default-font-size)

;; Fix italics.
(set-face-attribute 'italic nil :family "Hack" :slant 'italic)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Hack" :height
                    ns/default-variable-font-size :weight 'regular)

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
