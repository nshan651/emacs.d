(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds rss-feeds))
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :feed-url "youtube\\.com"
;; :add '(video youtube)))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package org-caldav
  :ensure t
  :custom
  ;; URL of the caldav server
  (org-caldav-url
   "http://nextcloud.nshan651.duckdns.org/nextcloud/remote.php/dav/calendars/nshan651")

  ;; calendar ID on server
  (org-caldav-calendar-id "personal")

  ;; Org filename where new entries from calendar stored
  (org-caldav-inbox "~/org/agenda/todo.org")

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
