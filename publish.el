(require 'ox-publish)

(setq org-publish-project-alist
      '(("blog"
	 :base-directory "~/Code/agumonkey.github.io/blog/src/"
	 :base-extension "org"
	 :publishing-directory "~/Code/agumonkey.github.io/blog/dest/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t
	 :auto-sitemap t
	 :sitemap-filename "sitemap.org"
	 :sitemap-title "Sitemap")
	("main"
	 :components ("blog"))))
