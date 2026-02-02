(defun ns/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun ns/org-mode-setup ()
  (org-indent-mode)
  (flyspell-mode)
  ;; (variable-pitch-mode)
  (visual-line-mode 1))

(use-package org
  :ensure nil ; Built-in to emacs, do not install.
  :commands (org-capture org-agenda)
  :hook
  (org-mode . ns/org-mode-setup)
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-directory "~/ark/org")
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-hide-block-startup nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  (setq org-modules
        '(org-habit))

  ;; Resize latex figures
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))

  (setq org-habit-graph-column 60)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (ns/org-font-setup))

;; Open the agenda file as the only window
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-files
      '("~/ark/org/agenda/todo.org"       ; General unsorted todo items.
        "~/ark/org/agenda/inbox.org"      ; TBD!
        "~/ark/org/agenda/contacts.org"   ; Contacts list.
        "~/ark/org/agenda/projects.org"   ; Project mgmt.
        "~/ark/org/agenda/cron.org"       ; Recurring events/habits.
        ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "PROJECT(x)" "|" "COMPLETED(c)" "CANC(c)")
        (sequence "BUG(B)" "TRIAGE(T)" "FIX(F)" "|" "RESOLVED(R)" "INVALID(I)" "CANC(c)")
        (sequence "GOAL(g)" "|" "DONE(d!)")
        (sequence "CONTACT(C)")))

;; Check colors with `list-colors-display'
(setq org-todo-keyword-faces
      '(("CONTACT" . (:foreground "goldenrod" :weight bold))
        ("PROJECT" . (:foreground "steel blue" :weight bold))
        ("GOAL" . (:foreground "aquamarine" :weight bold))))

(defun ns/org-archive-targets()
  "Expand the contents of the archive dir."
  (directory-files (expand-file-name "agenda/archive" org-directory) t ".org"))

(setq org-refile-targets
      '((ns/org-archive-targets :maxlevel . 1)))

;; Put mutually exclusive tags inside the group blocks.
(setq org-tag-alist
      '((:startgroup)
        ("location")
        (:grouptags)
        ("@home" . ?H)
        ("@work" . ?W)
        (:endgroup)

        ("context")
        (:grouptags)
        ("@computer" . ?H)
        ("@errands" . ?W)
        (:endgroup)

        ("@errand" . ?E)
        ("@investing" . ?I)
        ("appointment" . ?a)
        ("agenda" . ?A)
        ("followup" . ?f)
        ("planning" . ?p)
        ("publish" . ?P)
        ("batch" . ?b)
        ("note" . ?n)
        ("idea" . ?i)))

;; Configure custom agenda views
;; More on agenda view commands at `https://emacsdocs.org/docs/org/Agenda-Commands'
(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-block-separator nil)
                      (org-agenda-overriding-header "Important Tasks Without a Date\n")))
          (agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-scheduled-past-days 0)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "\nToday's Agenda\n")))
          (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext Three Days\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      ;; We don't want to replicate the previous section's
                      ;; three days, so we start counting from the day after.
                      (org-agenda-start-day "+4d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming Deadlines (+14d)\n")))
          (todo "PROJECT"
                ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                 (org-agenda-block-separator nil)
                 (org-agenda-overriding-header "\nProjects\n")))
          ))

        ("d" "Dashboard"
         ((agenda ""
                  (
                   (org-agenda-span 7)
                   (org-deadline-warning-days 0)
                   (org-scheduled-past-days 0)
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Actions")
                 (org-agenda-max-todos nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-files '("~/ark/org/agenda/inbox.org"))
                 (org-agenda-text-search-extra-files nil)))
          (agenda "" ((org-agenda-span 14)
                      (org-agenda-start-day "+7d")
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-entry-types '(:deadline :scheduled))
                      (org-agenda-overriding-header "Upcoming Deadlines (+14d)")))
          ))
        ))

(setq org-capture-templates
      (let* ((without-time (concat ":PROPERTIES:\n"
                                   ":CAPTURED: %U\n"
                                   ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                                   ":END:\n\n"
                                   "%a\n%?"))
             (with-time (concat "DEADLINE: %^T\n"
                                ":PROPERTIES:\n"
                                ":CAPTURED: %U\n"
                                ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                                ":APPT_WARNTIME: 20\n"
                                ":END:\n\n"
                                "%a%?")))
        `(("t" "Tasks")

          ;; ("tt" "Task" entry (file "~/ark/org/agenda/todo.org")
          ;;  "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("tt" "Task to do" entry
           (file+headline "agenda/todo.org" "All tasks")
           ,(concat "* TODO %^{Title} %^g\n" without-time)
           :empty-lines-after 1)

          ("ts" "Clocked Entry Subtask" entry (clock)
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("tr" "Recurring Task" entry (file "~/ark/org/agenda/cron.org")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ;; Projects
          ("p" "Projects")
          ("pt" "Task" entry (file "~/ark/org/agenda/projects.org")
           "* PROJECT %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("ps" "Clocked Entry Subtask" entry (clock)
           "* PROJECT %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("pr" "Pull Request" entry (file "~/ark/org/agenda/projects.org")
           "* TODO %?\n  :PROPERTIES:\n:DATE: %U\n:LINK: %^L \n:END:" :empty-lines 1)

          ;; Contacts
          ("c" "Contacts" entry (file "~/ark/org/agenda/contacts.org")
           "* CONTACT %^{Name}\n:PROPERTIES:\n:BIRTHDAY: %^{Specify birthday}t\n:PHONE: %^{Phone number}\n:END:\n%?" :empty-lines 1)

          )))

(define-key global-map (kbd "C-c j")
            (lambda () (interactive) (org-capture nil "jj")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun ns/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ns/org-mode-visual-fill))

(use-package org-appear
   :hook (org-mode . org-appear-mode))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  (evil-org-agenda-set-keys))

(ns/leader-spc 'override
  "o"   '(:ignore t :wk "org mode")

  "oi"  '(:ignore t :wk "insert")
  "oil" '(org-insert-link :wk "insert link")

  "oo"  '(org-agenda :wk "open agenda")

  "on"  '(org-toggle-narrow-to-subtree :wk "toggle narrow")
  "os"  '(consult-org-roam-search :wk "search notes")
  ;; "oa"  '(org-agenda :wk "status")
  "ot"  '(org-todo-list :wk "todos")
  "oc"  '(org-capture t :wk "capture")
  "ox"  '(org-export-dispatch t :wk "export"))

(defun ns/open-custom-agenda ()
  "Open the custom agenda view."
  (interactive)
  (org-agenda nil "d"))

(general-def 'override
  :prefix "C-c"
  "a" '(org-agenda :wk "org agenda")
  "c" '(org-capture :wk "org capture")
  "d" '(ns/open-custom-agenda :wk "main agenda dashboard")
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '(
        (emacs-lisp . t)
        (python . t)
        (shell . t)
        (C . t)
        (dot . t)
       ))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))
;; Disable execution confirmations
(setq org-confirm-babel-evaluate nil)

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (let ((templates '(("sh"  . "src sh")
                     ("el"  . "src emacs-lisp")
                     ("ini" . "src emacs-lisp :tangle \"init.el\" :mkdirp yes")
                     ("vim" . "src vim")
                     ("py"  . "src python")
                     ("rs"  . "src rust")
                     ("cs"  . "src csharp")
                     ("sql"  . "src sql")
                     ("y"  . "src yaml")
                     ;; Leetcode snippet
                     ("leet" .
                      "src C++ :includes <iostream> :flags -I./src/util -std=c++20 :tangle src/ .cpp")

                     ("cpp" . "src C++ :includes <iostream>"))))
    (dolist (template templates)
      (push template org-structure-template-alist))))

(use-package org-journal
  :custom
  (org-journal-dir "~/ark/org/journal/"))

(ns/leader-spc 'override
  "oj"  '(:ignore t :wk "org journal")
  "ojn" '(org-journal-new-entry :wk "new journal entry")
  "ojs" '(org-journal-search-forever :wk "search journal"))

(use-package org-drill)
