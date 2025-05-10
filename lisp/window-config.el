(use-package perspective
  :custom
  (persp-initial-frame-name "main")
  (persp-mode-prefix-key (kbd "C-a"))
  :init
  (unless (equal persp-mode t)
    (persp-mode)))

(ns/leader-ca 'persp-mode-map
  "s"  '(persp-switch :wk "query or create persp")
  "k"  '(persp-kill :wk "kill a persp")
  "r"  '(persp-rename :wk "rename a persp")
  ;; Buffer management
  "a"  '(persp-add-buffer :wk "add buffer to current persp")
  "A"  '(persp-set-buffer :wk "add buffer to current persp, remove from others")
  "b"  '(persp-switch-to-buffer :wk "switch to buffer, global by default")
  ;; Switch perspectives
  "n"  '(persp-next :wk "next persp")
  "p"  '(persp-prev :wk "previous persp")
  ;; Merging and importing
  "m"  '(persp-merge :wk "merge buffers into another persp")
  "u"  '(persp-unmerge :wk "undo effects of `persp-merge'")
  "i"  '(persp-import :wk "import a given perspective from another frame")
  ;; Save and load perspectives
  "C-s"  '(persp-state-save :wk "Save all persps in all frames to a file")
  "C-l"  '(persp-state-load :wk "load all persps from a file")
)

(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*\\*Messages"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1))                ; For echo area hints

(general-def 'override
  "C-`" '(popper-toggle :wk "toggle popup")
  "M-`" '(popper-cycle :wk "cycle through popups")
  "C-M-`" '(popper-toggle-type :wk "change current buffer's toggle type"))
