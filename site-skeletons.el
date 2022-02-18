;;; site-skeletons.el --- Skeletons for managing my website

;; Copyright (C) 2022, Jacob Stannix

;; Author: Jacob Stannix
;; Created: 18 Feb 2022

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

;; Skeletons for managing my website

;;; Code:

;;;###autoload
(defvar site-skeletons-suppress-auto-insert-mode 'nil
  "When non-nil ’auto-insert-mode’ will NOT be enabled when the library is loaded")

;;;###autoload
(defvar site-skeletons-turn-off-auto-insert-mode-with-unload 't
  "When ’nil’ disables ’auto-insert-mode’ when ’site-skeletons-unload-function’ is run")

(define-prefix-command 'site-skeletons-prefix 'site-skeletons-prefix-map "Site Skeletons")

(with-eval-after-load "autoinsert"
  (setq site-skeletons-base-org-regex
	(cons (concat "^" (getenv "HOME") "/website/.*[.]org") "org insert web heading"))
  
  (define-auto-insert
    site-skeletons-base-org-regex
    '("Page Title: "
      "#+TITLE: " str \n
      "#+OPTIONS: toc:nil timestamp:nil author:nil title:" (skeleton-read "Include Title? ") \n
      "#+OPTIONS: date:nil num:nil html-postamble:nil html-style:nil" \n
      "#+HTML_DOCTYPE: html5" \n
      '(setq v1 (skeleton-read "Top Directory? "))
      "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"" v1 "styles/sidebar.css\"/>" \n
      "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\"" v1 "styles/site.css\"/>" \n -))

  (setq site-skeletons-blog-org-regex
	(cons (concat "^" (getenv "HOME") "/website/blog/posts/\\.org") "org insert blog post heading"))

  (define-auto-insert
    site-skeletons-blog-org-regex
    '("Post Title: "
      "#+TITLE: " str \n
      "#+OPTIONS: toc:nil timestamps:nil author:nil" \n
      "#+OPTIONS: date:nil num:nil html-postamble:nil html-style:nil" \n
      "#+HTML_DOCTYPE: html5" \n
      '(setq v1 (skeleton-read "Top Directory? "))
      "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"" v1 "styles/sidebar.css\"/>" \n
      '(setq v2 (y-or-n-p "Include site stylesheet? "))
      (when v2
	(concat "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\"" v1 "sytles/site.css\"/>\n"))
      "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\"" v1 "styles/org.css\"/>\n")))


(define-skeleton site-skeletons-css
  "inserts an org formated css line"
  ""
  "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" href=\""
  (skeleton-read "Top Directory? ") "styles/"
  (skeleton-read "Css File? ") "\"/>" \n -)

(define-key site-skeletons-prefix-map (kbd "c") #'site-skeletons-css)

(unless site-skeletons-suppress-auto-insert-mode
  (auto-insert-mode t))

(defun site-skeletons-unload-function ()
  "Unloads ’site-skeletons’"
    (setq auto-insert-alist (assq-delete-all site-skeletons-base-org-regex auto-insert-alist))
    (setq auto-insert-alist (assq-delete-all site-skeletons-blog-org-regex auto-insert-alist))
    (makunbound 'site-skeletons-prefix-map)
    (fmakunbound 'site-skeletons-prefix)
    (makunbound 'site-skeletons-base-org-regex)
    (makunbound 'site-skeletons-blog-org-regex)
    (when site-skeletons-turn-off-auto-insert-mode-with-unload
      (auto-insert-mode -1)))

(provide 'site-skeletons)
;;; site-skeletons.el ends here

;; Local Variables:
;; electric-quote-mode: t
;; End:
