SITE_ROOT = site-files
HOST = root@jakestannix.org:/var/www/personal
RSYNC_COMMAND = rsync -vrP --delete-after $(SITE_ROOT)/ $(HOST)
EMACS_PUBLISH_SCRIPT = ./publish.el
EMACS_COMMAND = emacs -q -Q -nw --script $(EMACS_PUBLISH_SCRIPT)
BLOG_DIR = blog/posts/
css_files = $(addprefix styles/,org.css sidebar.css site.css test.css)
blog_files = $(addprefix $(BLOG_DIR),writing-a-website-with-org-mode.org)
.PHONY: site force update

$(SITE_ROOT)/index.html: index.org
	$(EMACS_COMMAND)

index.org: blog/index.org $(css_files)

blog/index.org: $(blog_files)

site: 
	$(RSYNC_COMMAND)

force:
	$(EMACS_COMMAND)

update:
	$(EMACS_COMMAND)
	$(RSYNC_COMMAND)
