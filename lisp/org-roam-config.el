(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/ark/org"))
  (org-roam-dailies-directory "~/ark/org/journal")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode)

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(use-package consult-org-roam
 :after org-roam
 :init
 ;; (require 'consult-org-roam)
 ;; Activate the minor mode
 (consult-org-roam-mode 1)
 :custom
 ;; Use `ripgrep' for searching with `consult-org-roam-search'
 (consult-org-roam-grep-func #'consult-ripgrep)
 ;; Configure a custom narrow key for `consult-buffer'
 (consult-org-roam-buffer-narrow-key ?r)
 ;; Display org-roam buffers right after non-org-roam buffers
 ;; in consult-buffer (and not down at the bottom)
 (consult-org-roam-buffer-after-buffers t)
 :config
 ;; Eventually suppress previewing for certain functions
 ;; (consult-customize
 ;;  consult-org-roam-forward-links
 ;;  :preview-key "M-.")
 :bind
 ;; Define some convenient keybindings as an addition
 ("C-c n e" . consult-org-roam-file-find)
 ("C-c n b" . consult-org-roam-backlinks)
 ("C-c n B" . consult-org-roam-backlinks-recursive)
 ("C-c n l" . consult-org-roam-forward-links)
 ("C-c n r" . consult-org-roam-search))

;; Keybindings
(ns/leader-spc
  "n"  '(:ignore t :wk "org node selection")
  "nl" '(org-roam-buffer-toggle :wk "org roam buffer toggle")
  "nf" '(org-roam-node-find :wk "org roam buffer find")
  ;; "ng" '(org-roam-graph :wk "org roam graph")
  "ni" '(org-roam-node-insert :wk "org roam insert")
  "nc" '(org-roam-capture :wk "org roam capture")
  "nj" '(org-roam-dailies-capture-today :wk "org roam dailies capture today"))

(setq org-roam-capture-templates
  '(("m" "main" plain
      "%?"
      :if-new
      (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                 "#+title: ${title}\n#+options: tex:t toc:nil\n#+startup: inlineimages latexpreview\n")
      :immediate-finish t
      :unnarrowed t)
      ("r" "reference" plain "%?"
      :if-new
      (file+head "reference/${title}.org"
                 "#+title: ${title}\n#+options: tex:t toc:nil\n#+startup: inlineimages latexpreview\n")
      :immediate-finish t
      :unnarrowed t)
      ("a" "article" plain "%?"
      :if-new
      (file+head "articles/${title}.org"
                 "#+title: ${title}\n#+options: tex:t toc:nil\n#+startup: inlineimages latexpreview\n#+filetags: :article:\n")
      :immediate-finish t
      :unnarrowed t)))

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name
            (org-roam-node-file node)
            org-roam-directory))))
      (error "")))
  )

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :disabled t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-default-view '2d
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(ns/leader-spc
  "ng" '(org-roam-ui-mode :wk "org roam ui graph"))
