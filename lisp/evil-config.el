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
    ;; (setq evil-undo-system 'undo-tree)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

    ;; Define key bindings for SPC followed by a single key
    (define-key evil-normal-state-map (kbd "SPC f") 'find-file)
    (define-key evil-normal-state-map (kbd "SPC p") 'projectile-command-map)
    (define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
    (define-key evil-normal-state-map (kbd "SPC s") 'counsel-projectile-rg)

    ;; Background transparency
    ;(define-key evil-normal-state-map (kbd "SPC z") 'toggle-transparency) 

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

    ;; Org Roam
    (define-key evil-normal-state-map (kbd "SPC n l") 'org-roam-buffer-toggle)
    (define-key evil-normal-state-map (kbd "SPC n f") 'org-roam-node-find)
    (define-key evil-normal-state-map (kbd "SPC n i") 'org-roam-node-insert)

    ;; Manage buffers
    (define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
    (define-key evil-normal-state-map (kbd "SPC eb") 'eval-buffer)

    ;; Manage windows
    (evil-global-set-key 'normal (kbd "C-a <backspace>") 'delete-window)
    (evil-global-set-key 'normal (kbd "C-a \\") 'split-window-right)
    (evil-global-set-key 'normal (kbd "C-a -") 'split-window-below)
    ;; Windmove keys for additional window navigation
    (evil-global-set-key 'normal (kbd "C-a h")  'windmove-left)
    (evil-global-set-key 'normal (kbd "C-a l")  'windmove-right)
    (evil-global-set-key 'normal (kbd "C-a k")  'windmove-up)
    (evil-global-set-key 'normal (kbd "C-a j")  'windmove-down)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
