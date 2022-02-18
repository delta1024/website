;;; publish.el --- A script for generating a website from org documents

;; Copyright (C) 2022 Jacob Stannix

;; Creater: Jacob Stannix
;; Date: 16 Feb 2022
;; Package-Requires: ((emacs "28.0.91") (org "9.4.6") (dired))

;; This file is part of jakestannix.org.

;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public
;; License along with this file. If not, see
;; https://www.gnu.org/licenses/.

;;; Commentary:

;; A helper script for make to generate the website

;;; Code:

(require 'org)
(require 'dired)
(setq org-publish-use-timestamps-flag 'nil)

(setq exclude
  (let ((org-headers '("settings.org"
		       "sidebar.org"))
	(final-string 'nil))

    (dolist (file org-headers)
      (setq final-string (concat file "\\|" final-string)))

    (string-trim-right final-string "\\\\|")))

(setq org-publish-project-alist
      `(("index"
	 :base-directory "~/website"
	 :base-extension "org"
	 :publishing-directory "~/website/site-dir"
         :publishing-function org-html-publish-to-html)
         
	("images"
	 :base-directory "~/website/media"
	 :base-extension "jpg\\|gif\\|png"
	 :publishing-directory "~/website/site-dir/media"
	 :publishing-function org-publish-attachment)

	("blog_index"
	 :base-directory "~/website/blog"
	 :base-extension "org"
	 :publishing-directory "~/website/site-dir/blog"
	 :publishing-function org-html-publish-to-html)

	("website" :components ("index" "images" "blog_index"))
	
    	("blog"
	 :base-directory "~/website/blog/posts/"
	 :base-extension "org"
	 :publishing-directory "~/website/site-dir/blog/posts"
	 :publishing-function org-html-publish-to-html)

	("css"
	 :base-directory "~/website/styles"
	 :base-extension "css"
	 :publishing-directory "~/website/site-dir/styles"
	 :publishing-function org-publish-attachment)
	
	("all" :components ("website" "blog" "css"))))


(setq command-switch-alist '(("--generate-section" . (lambda (arg)
						       (let ((option (pop command-line-args-left)))
							 (org-publish option))))))

;;; publish.el ends here
