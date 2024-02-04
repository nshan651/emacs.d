(load (locate-user-emacs-file
       "etc/feed.el"))

(use-package elfeed
  :ensure t)

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

(setq elfeed-feeds rss-feeds)
