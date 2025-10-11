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
  (rcirc-server-alist `(("irc.libera.chat"
			 :channels ("#emacs" "#guix")
                         :port 6697
                         :encryption tls)))
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

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'qwen3:1.7b-ns)
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
          :models '(openai/gpt-oss-120b
                    moonshotai/kimi-k2-instruct
                    llama-3.1-70b-versatile
                    qwen/qwen3-32b
                    openai/gpt-oss-20b)))

  (setq ns/gptel-gemini-backend
        (gptel-make-gemini "gemini"
          :stream t
          :key (lambda () (auth-source-pass-get 'secret "gemini/key"))
          :models '(gemini-2.0-flash-lite
                    gemini-2.5-flash
                    (gemini-2.5-pro-latest
                     :description
                     "Complex reasoning tasks, problem solving and data extraction"
                     :capabilities (tool json)
                     :mime-types
                     ("image/jpeg" "image/png" "image/webp" "image/heic")))))


  ;; Set the defualt model backend.
  ;; (setq gptel-backend ns/gptel-ollama-backend)
  (setq gptel-backend ns/gptel-gemini-backend)

  ;; Configure system prompts.
  (gptel-make-preset "Incremental Reasoning"
    :system-message "Let's think step by step to assure we arrive at the correct answer."
    :backend "shodan")

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


;; Define keyboard shortcuts.
(ns/leader-spc
  "g"  '(:ignore t :wk "gptel")
  "gb" '(gptel-abort :wk "gptel abort")
  "gg" '(gptel-mode :wk "gptel mode")
  "gp" '(gptel :wk "gptel prompt")
  "gr" '(gptel-rewrite :wk "gptel rewrite")
  "gs" '(gptel-send :wk "gptel send")
  "gm" '(gptel-menu :wk "gptel menu")
  "ga" '(gptel-add :wk "gptel add")
  "gf" '(gptel-add-file :wk "gptel add file"))
