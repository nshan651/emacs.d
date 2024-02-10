(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds rss-feeds))
;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :feed-url "youtube\\.com"
;; :add '(video youtube)))
