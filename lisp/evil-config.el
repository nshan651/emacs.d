;; Bind C-x C-b to ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (use-package evil
      :init
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll t)
      (setq evil-want-C-i-jump nil)
      (setq evil-respect-visual-line-mode t)
      :config
      (evil-mode 1)
      (evil-set-initial-state 'messages-buffer-mode 'normal)
      (evil-set-initial-state 'dashboard-mode 'normal)

      ;; Org Agenda
      (define-key evil-normal-state-map (kbd "SPC o") 'org-agenda)

      ;; Open TODO agenda file as a pop-up buffer
      (define-key evil-normal-state-map (kbd "SPC r")
        (lambda ()
          (interactive)
          (let ((file "~/org/agenda/todo.org")
                  (pop-up-buffer "*TODO*"))
              (pop-to-buffer pop-up-buffer)
              (find-file file))))

      (defun open-custom-agenda ()
        "Open the custom agenda view."
        (interactive)
        (org-agenda nil "A"))

      (define-key evil-normal-state-map (kbd "SPC a") 'open-custom-agenda)
)

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
  "tt" '(consult-theme :which-key "Choose a theme"))

(ns/leader-spc
  "f"  'find-file
  ;; "b"  'switch-to-buffer
  "k"  'kill-buffer
  "eb" 'eval-buffer)

;; Manage windows
(ns/leader-ca
  "<backspace>" 'delete-window
  "\\"          'split-window-right
  "-"           'split-window-below
  ;; Windmove keys for additional window navigation
  "h"           'windmove-left
  "l"           'windmove-right
  "k"           'windmove-up
  "j"           'windmove-down)

(use-package evil-nerd-commenter
  :general ("M-/" 'evilnc-comment-or-uncomment-lines))
