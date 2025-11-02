(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-respect-visual-line-mode t)
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
  ;; General-purpose leader prefix.
  (general-create-definer ns/leader-spc
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")
  ;; Cosmetics.
  (general-create-definer ns/leader-t
    :states 'normal
    :keymaps 'override
    :prefix "t")
  ;; Read-only docs.
  (general-create-definer ns/leader-r
    :states 'motion
    :prefix "r")
  ;; Magit operations.
  (general-create-definer ns/leader-m
    :states 'normal
    :prefix "m")
  ;; Window management.
  (general-create-definer ns/leader-ca
    :states 'normal
    :keymaps 'override
    :prefix "C-a")
  ;; Executive.
  (general-create-definer ns/leader-comma
    :states 'normal
    :prefix ","))
  ;; Misc.
  (general-create-definer ns/leader-ct
    :keymaps '(insert normal)
    :keymaps 'override
    :prefix "C-t")

;; Bind C-x C-b to ibuffer.
(keymap-global-set "C-x C-b" 'ibuffer)
;; Make ESC quit prompts.
(keymap-global-set "<escape>" 'keyboard-escape-quit)
;; Sensible copy-paste behavior.
(keymap-global-set "C-S-c" 'copy-region-as-kill)
(keymap-global-set "C-S-v" 'yank)

;; Use visual line motions even outside of visual-line-mode buffers
(general-def 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(ns/leader-spc
  "f"  'find-file
  "k"  'kill-buffer
  "eb" 'eval-buffer)

;; Compiling and recompiling.
(general-def 'normal
  :prefix "g"
  "c" 'compile
  "r" 'recompile)

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

(use-package evil-nerd-commenter
  :general ("M-/" 'evilnc-comment-or-uncomment-lines))
