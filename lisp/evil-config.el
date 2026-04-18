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
    :states '(insert visual normal)
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
  "h"           'windmove-left
  "l"           'windmove-right
  "k"           'windmove-up
  "j"           'windmove-down)

(defun ns/move-border (direction delta)
  "Resize the current window in DIRECTION by DELTA."
  (pcase direction
    ('right
     (if (window-in-direction 'right)
         (enlarge-window-horizontally delta)
       (shrink-window-horizontally delta)))
    ('left
     (if (window-in-direction 'right)
         (shrink-window-horizontally delta)
       (enlarge-window-horizontally delta)))
    ('down
     (if (window-in-direction 'below)
         (enlarge-window delta)
       (shrink-window delta)))
    ('up
     (if (window-in-direction 'below)
         (shrink-window delta)
       (enlarge-window delta)))))

(ns/leader-ca 'override
  "C-h" (lambda () (interactive) (ns/move-border 'left  15))
  "C-l" (lambda () (interactive) (ns/move-border 'right 15))
  "C-j" (lambda () (interactive) (ns/move-border 'down  11))
  "C-k" (lambda () (interactive) (ns/move-border 'up    11)))

(use-package evil-nerd-commenter
  :general ("M-/" 'evilnc-comment-or-uncomment-lines))
