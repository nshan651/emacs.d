(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds rss-feeds))
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :feed-url "youtube\\.com"
;; :add '(video youtube)))

(use-package rcirc
  :custom
  (rcirc-default-nick "nshan651")
  (rcirc-default-user-name "nshan651")
  (rcirc-default-full-name "ns")
  (rcirc-server-alist '(
                        ("irc.libera.chat"
			             :channels ("#emacs" "#guix")
                         :port 6697
                         :encryption tls
                         )))
  (rcirc-prompt "%t> ")
  (rcirc-timeout-seconds most-positive-fixnum)

  (rcirc-reconnect-delay 5)
  (rcirc-fill-column 90)
  (rcirc-track-ignore-server-buffer-flag t))

(use-package ox-hugo
  :after ox)

(use-package org-caldav
  :custom
  ;; URL of the caldav server
  (org-caldav-url
   "http://nextcloud.nshan651.duckdns.org/nextcloud/remote.php/dav/calendars/nshan651")

  ;; calendar ID on server
  (org-caldav-calendar-id "personal")

  ;; Org filename where new entries from calendar stored
  (org-caldav-inbox "~/ark/org/agenda/todo.org")

  ;; Additional Org files to check for calendar events
  (org-caldav-files nil)

  ;; Usually a good idea to set the timezone manually
  (org-icalendar-timezone "America/Chicago")

  ;; TODO
  (org-caldav-sync-todo t)
  (org-caldav-todo-deadline-schedule-warning-days t)

  (org-icalendar-include-todo 'all)
  ;; (org-icalendar-use-deadline 'todo-due)
  ;; (org-icalendar-use-scheduled 'todo-start)
  (org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
  (org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start))
  (org-icalendar-with-timestamps t)
)

;; (use-package guix)

(use-package org-present
  :bind
  (:map org-mode-map
              ("C-<f12>" . org-present)
              ("C-w f"   . org-present-toggle-one-big-page)))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'moonshotai/kimi-k2-instruct)
  :config

  ;; Set up model backends.
  (setq ns/gptel-ollama-backend (gptel-make-ollama "shodan"
                                  :host "shodan:11434" ;; Or use shodan.local for mDNS.
                                  :stream t
                                  :models '(deepseek-r1:1.5b-ns
                                            gemma3:1b
                                            gemma3:latest
                                            llama3.2:latest
                                            llama3.2-ns
                                            qwen3:1.7b-ns
                                            qwen3:1.7b
                                            qwen3:4b-ns)))

  (setq ns/gptel-openrouter-backend
        (gptel-make-openai "openrouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda () (auth-source-pass-get 'secret "openrouter/key"))
          :models '(deepseek/deepseek-chat-v3.1:free
                    openai/gpt-oss-20b:free
                    qwen/qwen3-coder:free
                    moonshotai/kimi-k2:free
                    )))

  (setq ns/gptel-groq-backend
        (gptel-make-openai "groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key (lambda () (auth-source-pass-get 'secret "groq/key"))
          :models '(moonshotai/kimi-k2-instruct
                    openai/gpt-oss-120b
                    llama-3.1-70b-versatile
                    qwen/qwen3-32b
                    openai/gpt-oss-20b)))

  (setq ns/gptel-gemini-backend
        (gptel-make-gemini "gemini"
          :stream t
          :key (lambda () (auth-source-pass-get 'secret "gemini/key"))
          :models '(
                    gemini-flash-latest
                    gemini-pro-latest
                    gemini-flash-lite-latest
                    gemini-2.5-flash
                    gemini-2.0-flash-lite
                    gemini-2.5-pro
                    )))


  ;; Set the defualt model backend.
  (setq gptel-backend ns/gptel-groq-backend)

  ;; Configure system prompts.
  (gptel-make-preset "Incremental Reasoning"
    :system-message "Let's think step by step to assure we arrive at the correct answer."
    :backend      "groq"
    :model        'moonshotai/kimi-k2-instruct)

  (gptel-make-preset 'proofreader
    :description "Preset for proofreading tasks."
    :backend "gemini"
    :model 'gemini-2.0-flash
    :tools '("read_buffer" "spell_check" "grammar_check")
    :temperature 0.7
    :use-context 'system)

  (gptel-make-preset 'websearch
    :description  "gemini with basic web search capability."
    :backend      "gemini"
    :model        'gemini-2.5-flash
    :tools        '("search_web" "read_url" "get_youtube_meta"))

  (gptel-make-preset 'resume
    :system-message "Use the following resume to answer interview questions in a thoughtful way."
    :backend "shodan"
    :pre (lambda () (gptel-add-file "~/git/interviews/resume.org")))
  )

(defun ns/gptel-select-backend ()
  "Select a gptel backend and update `gptel-backend`."
  (interactive)
  (let* ((backends '(("shodan" . ns/gptel-ollama-backend)
                     ("openrouter" . ns/gptel-openrouter-backend)
                     ("groq" . ns/gptel-groq-backend)
                     ("gemini" . ns/gptel-gemini-backend)
                     ))
         (choice (completing-read "Select gptel backend: " (mapcar #'car backends)))
         (backend (cdr (assoc choice backends))))
    (when backend
      (setq gptel-backend (symbol-value backend))
      (message "gptel backend set to: %s" choice))))

(defun ns/quickdraw (prompt)
  (gptel-request prompt
    :callback
    (lambda (response info)
      (when response
        (with-current-buffer (get-buffer-create "*quickdraw*")
          (org-mode)
          (goto-char (point-max))
          (unless (bobp)
            (insert (format "\n\n")))
          (insert (format "* %s\n" (format-time-string "%F %T")))
          (insert response)
          (display-buffer (current-buffer)))))))

(defun ns/quickdraw-selection ()
  "Quick analysis of selected region, results go to *quickdraw* buffer."
  (interactive)
  (unless (region-active-p)
    (user-error "No region selected"))
  (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format "Help me quickly understand the essence of the following selection:\n%s" selected-text)))
    (ns/quickdraw prompt)
    ))

(defun ns/quickdraw-prompt ()
  "Send a prompt from minibuffer to *quickdraw* buffer."
  (interactive)
  (let ((prompt (read-string "Quickdraw: ")))
    (when (string-empty-p prompt)
      (user-error "Empty prompt"))
    (ns/quickdraw prompt)
    ))

;; Unset `evil-scroll-line-down'.
(keymap-global-unset "C-e")
(with-eval-after-load 'evil
  (keymap-set evil-normal-state-map "C-e" nil)
  (keymap-set evil-insert-state-map "C-e" nil)
  (keymap-set evil-motion-state-map "C-e" nil))

;; TODO: add this in to a general-def?
(with-eval-after-load 'evil
  (keymap-set evil-visual-state-map "C-e q" #'ns/quickdraw-selection)
  (keymap-global-set "C-e q" #'ns/quickdraw-prompt))

;; Gptel keyboard shortcuts.
(general-def 'override
  :prefix "C-e"
  "b" '(gptel-abort :wk "gptel abort")
  "g" '(gptel-mode :wk "gptel mode")
  "p" '(gptel :wk "gptel prompt")
  "r" '(gptel-rewrite :wk "gptel rewrite")
  "s" '(gptel-send :wk "gptel send")
  "m" '(gptel-menu :wk "gptel menu")
  "a" '(gptel-add :wk "gptel add")
  "f" '(gptel-add-file :wk "gptel add file"))
