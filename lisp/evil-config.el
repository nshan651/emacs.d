;; Bind C-x C-b to ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :config
  ;; Create definer keys to map keys with
  (general-create-definer ns/leader-spc
    :states 'normal
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer ns/leader-t
    :states 'normal
    :keymaps 'override
    :prefix "t")
  (general-create-definer ns/leader-r
    :states 'motion
    :prefix "r")
  (general-create-definer ns/leader-m
    :states 'normal
    :prefix "m")
  (general-create-definer ns/leader-ca
    :states 'normal
    :keymaps 'override
    :prefix "C-a")
  (general-create-definer ns/leader-ct
    :keymaps '(insert normal)
    :keymaps 'override
    :prefix "C-t")
  (general-create-definer ns/leader-comma
    :states 'normal
    :prefix ","))

;; Use visual line motions even outside of visual-line-mode buffers
(general-def 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

;; Choose a theme
;; TODO move this to consult
(ns/leader-t
  "t" '(consult-theme :wk "Choose a theme"))

(ns/leader-spc
  "f"  'find-file
  ;; "b"  'switch-to-buffer
  "k"  'kill-buffer
  "eb" 'eval-buffer)

;; Manage windows
(ns/leader-ca 'override
  "<backspace>" 'delete-window
  "\\"          'split-window-right
  "-"           'split-window-below
  ;; Windmove keys for additional window navigation
  "h"          'windmove-left
  "l"           'windmove-right
  "k"          'windmove-up
  "j"           'windmove-down)
;; Window resizing
;; "C-h"        (lambda () (interactive) (shrink-window-horizontally 21))
;; "C-l"         (lambda () (interactive) (enlarge-window-horizontally 21))
;; "C-j"         (lambda () (interactive) (enlarge-window 11))
;; "C-k"        (lambda () (interactive) (shrink-window 11))

;; https://www.emacswiki.org/emacs/WindowResize
(defun ns/define-x-pos ()
  "Find the window's position on the x-axis."
  (let* ((win-edges (window-edges))
         (x-min (nth 0 win-edges))
         (x-max (nth 2 win-edges))
         (max-width (+ 2 (frame-width))))
    (cond
     ((equal max-width x-max)
      "right")
     ((and (> x-min 0) (< x-max max-width))
      "mid")
     (t "left"))))

(defun ns/win-resize-left ()
  (interactive)
  (let ((x-pos (ns/define-x-pos)))
    (cond
     ((equal "right" x-pos)
      (enlarge-window-horizontally +15))
     (t (enlarge-window-horizontally -15))
     ))
  )

(defun ns/win-resize-right ()
  (interactive)
  (let ((x-pos (ns/define-x-pos)))
    (cond
     ((equal "right" x-pos)
      (enlarge-window-horizontally -15))
     (t (enlarge-window-horizontally +15))
     ))
  )

(ns/leader-ca 'override
  "C-h"       (lambda () (interactive)
                (ns/win-resize-left))
  "C-l"        (lambda () (interactive)
                 (ns/win-resize-right))
  "C-j"         (lambda () (interactive) (enlarge-window 11))
  "C-k"        (lambda () (interactive) (shrink-window 11)))

;; Compiling and recompiling.
(general-def 'normal
  :prefix "g"
  "c" 'compile
  "r" 'recompile)

(use-package evil-nerd-commenter
  :general ("M-/" 'evilnc-comment-or-uncomment-lines))
