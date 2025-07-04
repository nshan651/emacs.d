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
  (gptel-model 'llama3.2:latest)
  :config
  (setq gptel-backend (gptel-make-ollama "shodan"
                   :host "shodan:11434" ;; Or use shodan.local for mDNS.
                   :stream t
                   :models '(gemma3:1b
                             gemma3:latest
                             llama3.2:latest
                             phi4-mini:latest))))

(ns/leader-spc
  "g"  '(:ignore t :wk "gptel")
  "gg" '(gptel-mode :wk "gptel mode")
  "gp" '(gptel :wk "gptel prompt")
  "gr" '(gptel-rewrite :wk "gptel rewrite")
  "gs" '(gptel-send :wk "gptel send")
  "gm" '(gptel-menu :wk "gptel menu")
  "ga" '(gptel-add :wk "gptel add")
  "ga" '(gptel-add-file :wk "gptel add file")
  )
