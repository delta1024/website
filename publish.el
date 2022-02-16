;;; publish.el --- A script for generating a website from org documents

;; Copyright (C) 2022 Jacob Stannix

;; Creater: Jacob Stannix
;; Date: 16 Feb 2022
;; Package-Requires: ((emacs "28.0.91") (org "9.4.6"))

;;; Commentary:

;; A simple script for publishing org documents into a website and
;; syncing the generated files to the web server with rsync.

;;; Code:

(require 'org)
(require 'dired)

(shell-command "find ./site-files ! -name 'index.html' -type d -exec rm -rf {} +")
(setq org-publish-project-alist
      '(("orgfiles"
	 :base-directory "~/website"
	 :base-extension "org"
	 :exclude "writing-a-website-with-org-mode.org\\|settings.org\\|sidebar.org"
	 :publishing-directory "~/website/site-files"
         :publishing-function org-html-publish-to-html
         :recursive t)

	("images"
	 :base-directory "~/website/media"
	 :base-extension "jpg\\|gif\\|png"
	 :publishing-directory "~/website/site-files/media"
	 :publishing-function org-publish-attachment)

	("other"
	 :base-directory "~/website/styles"
	 :base-extension "css"
	 :publishing-directory "~/website/site-files/styles"
	 :publishing-function org-publish-attachment)

	("website" :components ("orgfiles" "images" "other"))))

(org-publish "website" t)

;;; publish.el ends here
