(setq rss-feeds
      '(
	;; === Blogs ===
	"http://www.50ply.com/atom.xml"
	"https://danluu.com/atom.xml"
	"https://dynomight.net/feed.xml"
	"https://evalapply.org/index.xml"
	"https://handsandcities.com/rss"
	"https://irreal.org/blog/?feed=rss2"
	"http://jrsinclair.com/index.rss"
	"http://joeyh.name/blog/index.rss"
	"http://jsomers.net/blog/feed"
	"https://lukesmith.xyz/rss.xml"
	"https://mikemcquaid.com/atom.xml"
	"https://kevincox.ca/feed.atom"
	"https://www.lesswrong.com/feed.xml?view=curated-rss"
	"https://shlegeris.com/feed.xml"
	"https://stevelosh.com/rss.xml"
	"https://www.autodidacts.io/rss"
	"http://possiblywrong.wordpress.com/feed/"
	"http://www.devrand.org/feeds/posts/default"
	"https://nullprogram.com/blog/"
	"https://buttondown.email/hillelwayne/rss"
	"https://karthinks.com/index.xml"

	;; === Tech ===
	"https://www.archlinux.org/feeds/news/"
	"https://gizmodo.com/rss"
	"https://news.ycombinator.com/rss"
	"https://newatlas.com/index.rss"
	"https://www.notechmagazine.com/feed"
	"https://blog.janestreet.com/feed.xml"
	"https://lobste.rs/rss"
	"https://solar.lowtechmagazine.com/feeds/all-en.atom.xml"
	"https://www.theregister.com/on_prem/hpc/headlines.atom"
	"https://www.theregister.com/headlines.atom"
	"https://tilde.news/rss"
	"https://cheapskatesguide.org/cheapskates-guide-rss-feed.xml"
	"https://serokell.io/blog.rss.xml"
	"http://rss.slashdot.org/Slashdot/slashdotMain"
	"https://theconversation.com/us/technology/articles.atom"
	"https://www.osnews.com/feed/"
	"https://www.2600.com/rss.xml"

	;; === AI ===

	;; === Economics ===
	"https://fullstackeconomics.com/rss"
	"https://rss.stratechery.passport.online/feed/rss/8Lu26W627ebVKM2W9G7JCM"
	"https://theconversation.com/us/business/articles.atom"

	;; === Emacs ===
	"https://planet.emacslife.com/atom.xml"

	;; === Personal Finance ===

	;; === Educational ===
	"https://feeds.feedburner.com/brainpickings/rss"
	"http://feeds.nature.com/nature/rss/current"
	"https://nautil.us/rss"
	"https://www.rferl.org/api/zmoiie$kii"

	;; === Collections ===

	;; === Fediverse ===

	;; === History ===

	;; === Literature ===
	"https://pluralistic.net/feed/"
	"https://lithub.com/feed/"
	"https://singularityhub.com/feed/"
	"https://thecritic.co.uk/feed/"
	"https://thewalrus.ca/feed/"

	;; === Art ===
	"https://news.artnet.com/art-world/feed"

	;; === Comics ===
	"https://www.commitstrip.com/en/feed/"
	"https://xkcd.com/rss.xml"
	"https://explosm.net/rss.xml"

	;; === Cooking ===

	;; === Travel ===
	"https://www.atlasobscura.com/latest.rss"

	;; === Link Aggregators ===

	;; === Philosophy ===
	"http://feeds.feedburner.com/PhilosophyEtCetera"

	;; === Politics ===
	"https://reason.com/latest/feed/"

	;; === Chicago ===
	"https://chicagoboyz.net/feed"
	;; https://www.chicagocontrarian.com/atom
	;; https://www.chicagojazz.com/atom
	;; https://www.chicagojazz.com/atom.xml
	;; https://www.chicagocontrarian.com/atom.xml

	;; === Music ===

	;; === Videos ===
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCESLZhusAkFfsNsApnjF_Cg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC3ts8coMP645hZw9JSD3pqQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC8wKWWarusivFpIcUx9ilOw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCr3cBLTYmIK9kY0F_OdFWFQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC-AQKm7HUNMmxjdS371MSwg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCjREVt2ZJU8ql-NC9Gu-TJw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCaSCt8s_4nfkRglWCvNSDrg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCUk0AvSMU5CJ0wUqJf9TX8g"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCMlGfpWw-RUdWX_JbLCukXg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCvKRFNawVcuz4b9ihUTApCg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCWQaM7SpSECp9FELz-cHzuQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCxddeIv7GdHNcVPZI9JvGXQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCsBjURrPoezykLs9EqgamOA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCqJ-Xo29CKyLTjn6z2XwYAw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCJetJ7nDNLlEzDLXv7KIo0w"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC6szP4rcPWocLDPOJjzuzcg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCV58y_DbGkuYCNQC2OjJWOw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCutGiN7c5-CEFwm_ccixR3g"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC9nTrottgcMVWzIVW6cVqWw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCrx2zrPjhGRi9TwszZiLwEg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC_r8gFeezEBZVnazvbv75pQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCkCGANrihzExmu9QiqZpPlQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCmBXkjKD8w6bbjUzNjcDtQA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCeTfBygNb1TahcNpZyELO8g"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC9wO9GOGNPYywz3H20VGH1A"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCR-DXc1voovS8nhAvccRZhg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC5Gmg-VtFmnP8qLq8V7Pvtg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCyhnYIvIKK_--PiJXCMKxQQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCuvSqzfO_LV_QzHdmEj84SQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCGe8kzr_18FIOzjkmzGzzuA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC18YhnNvyrU2kTwCyj9p5ag"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCnHX5FjwtQpxkCGziuh4NJA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCl2mFZoRqjw_ELax4Yisf6w"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCb_sF2m3-2azOqeNEdMwQPw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCbzVRTkX3bzNZuBd9In4XyA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC6-rliFvsdCUTZndrZTQjMA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCTdw38Cw6jcm0atBPA39a0Q"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCJkMlOu7faDgqh4PfzbpLdg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCSh87zxGNu8q8iOInRK6E9w"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCUMwY9iS8oMyWDYIe6_RmoA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC0intLFzLaudFG-xAvUEO-A"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCowk8yXLosab5nCgxJdUiCw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC4EFgGdLCsUKXO4LzMfypMA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCivETRAS-l1zO3RaR9f8PjA"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCfHmyqCntYHQ81ZukNu66rg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCTDWASrT-T6wMBcZU4IXCKQ"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC3HPbvB6f58X_7SMIp6OPYw"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCEbYhDd6c6vngsF5PQpFVWg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCrqM0Ym_NbK1fqeQG2VIohg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCsnGwSIHyoYN0kiINAGUKxg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UCcefcZRL2oaA_uBNeo5UOWg"
	"https://www.youtube.com/feeds/videos.xml?channel_id=UC4EY_qnSeAP1xGsh61eOoJA"
	))

;; UNSORTED

;; https://nshan651.github.io/index.xml

;; https://avdi.codes/feed/
;; https://suricrasia.online/blog/feed.xml
;; https://cheapskatesguide.org/feed
;; https://danluu.com/atom
;; https://dynomight.net/feed
;; https://www.niamaelbassunie.com/essays?format=rss
;; https://gwern.substack.com/feed
;; https://jacobian.org/rss
;; https://blog.janestreet.com/feed.xml
;; https://joecarlsmith.com/rss.xml
;; https://jvns.ca/atom.xml
;; https://kevincox.ca/feed.atom
;; https://solar.lowtechmagazine.com/posts/index.xml
;; https://lukesmith.xyz/rss
;; https://mikemcquaid.com/atom.xml
;; https://feeds.feedburner.com/notechmagazine
;; https://pluralistic.net/feed
;; https://www.scattered-thoughts.net/atom.xml
;; https://joeyh.name/blog/index.rss
;; https://stevelosh.com/rss.xml
;; https://stratechery.com/feed
;; https://taylor.town/feed.xml
;; https://www.autodidacts.io/rss/
;; https://jsomers.net/blog/feed
;; https://www.marginalia.nu/log/index.xml
;; http://xahlee.info/comp/blog.xml

;; https://seekingalpha.com/feed/
;; https://www.bogleheads.org/forum/feed.php
;; https://www.fullstackeconomics.com/feed


;; https://news.artnet.com/feed/
;; https://chicagoboyz.net/feed
;; https://lithub.com/feed/
;; https://www.medievalists.net/rss
;; https://thecritic.co.uk/feed/
;; https://feeds.feedburner.com/brainpickings/rss
;; https://thewalrus.ca/feed/
;; https://www.thecollector.com/rss

;; https://xkcd.com/atom.xml

;; https://www.lesswrong.com/feed.xml

;; https://news.ycombinator.com/rss
;; https://lobste.rs/rss

;; https://stallman.org/rss/rss.xml

;; https://archlinux.org/feeds/news/
;; https://gizmodo.com/rss
;; https://nautil.us/feed/
;; https://newatlas.com/index.rss
;; https://singularityhub.com/feed/
;; https://www.theregister.com/headlines.atom

;; https://www.atlasobscura.com/feeds/latest
