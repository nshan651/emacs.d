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
  "b"  '(persp-switch-to-buffer* :wk "switch to persp buffer")
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

(ns/leader-spc
  "bb" '(persp-switch-to-buffer* :wk "switch to persp buffer"))

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

(defvar ns/formation-register-name
  "ns/windows-time-machine"
  "Base name of the register that ns uses to reverse `ns/delete-other-windows'.
Made perspective-specific by appending the current perspective name.")

(defvar ns/last-full-frame-window
  nil
  "Last window that was full-frame'd.
Perspective-local, see `ns/persp-setup-focus'.")

(defvar ns/last-full-frame-buffer
  nil
  "Last buffer that was full-frame'd.
Perspective-local, see `ns/persp-setup-focus'.")

(defun ns/formation-register ()
  "Return the register name for the current perspective.
Falls back to `ns/formation-register-name' when not in a perspective."
  (if (and (bound-and-true-p persp-mode) (persp-curr))
      (intern (format "%s/%s" ns/formation-register-name (persp-current-name)))
    ns/formation-register-name))

(defun ns/persp-reset-focus ()
  "Reset the full-frame state for a freshly created perspective."
  (setq ns/last-full-frame-window nil
        ns/last-full-frame-buffer nil))

(defun ns/persp-setup-focus ()
  "Make the focus state perspective-local.
Used by `ns/focus' so each perspective remembers its own split layout."
  (when (and (bound-and-true-p persp-mode) (persp-curr))
    (persp-make-variable-persp-local 'ns/last-full-frame-window)
    (persp-make-variable-persp-local 'ns/last-full-frame-buffer)
    (add-hook 'persp-created-hook #'ns/persp-reset-focus)))

(ns/persp-setup-focus)

(defun ns/delete-other-window ()
  "Save current window-buffer configuration and full-frame the current buffer."
  (setq ns/last-full-frame-window (selected-window))
  (setq ns/last-full-frame-buffer (current-buffer))
  (window-configuration-to-register (ns/formation-register))
  (delete-other-windows))

(defun ns/restore-other-windows ()
  "Restore the window configuration to prior to full-framing."
  (jump-to-register (ns/formation-register)))

(defun ns/focus ()
  "If the current frame has several windows, it will act as `delete-other-windows'.
If the current frame has one window,
	and it is the one that was last full-frame'd,
	and the buffer remained the same,
it will restore the window configuration to prior to full-framing."
  (interactive)
  (if (and (equal (selected-window) (next-window))
           (equal (selected-window) ns/last-full-frame-window)
           (equal (current-buffer) ns/last-full-frame-buffer))
      (ns/restore-other-windows)
    (ns/delete-other-window)))
