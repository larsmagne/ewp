;;; ewp.el --- Edit Wordpress -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

;; Ewp is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Ewp is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Ewp; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'dom)
(require 'metaweblog)
(require 'tabulated-list)

(defvar ewp-blog-address nil
  "The name/address of the blog, like my.example.blog.")

(defvar ewp-blog-id 1
  "The Wordpress ID of the blog, which is usually 1.")

(defvar ewp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\r" 'ewp-select-post)
    map))

(define-derived-mode ewp-list-mode tabulated-list-mode "ewp"
  "Major mode for listing Wordpress posts.

All normal editing commands are switched off.
\\<ewp-mode-map>

The following commands are available:

\\{ewp-mode-map}"
  (setq tabulated-list-format [("Status" 10 ewp-sort-status)
			       ("Date" 10 ewp-sort-date)
			       ("Title" 40 ewp-sort-title)]
	tabulated-list-sort-key (cons "Date" nil)
	tabulated-list-printer 'ewp-print-entry)
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun ewp-list-posts ()
  "List all the posts on the blog."
  (interactive)
  (switch-to-buffer (format "*%s posts*" ewp-blog-address))
  (let* ((auth (ewp-auth))
	 (inhibit-read-only t))
    (erase-buffer)
    (ewp-list-mode)
    (setq tabulated-list-entries
	  (ewp-get-posts (format "https://%s/xmlrpc.php" ewp-blog-address)
			 (getf auth :user) (funcall (getf auth :secret))
			 ewp-blog-id 100))
    (tabulated-list-print)))

(defun ewp-print-entry (id cols)
  "Insert a Wordpress entry at point."
  (insert ?\n))

(defun ewp-auth ()
  (let ((auth
         (nth 0 (auth-source-search
		 :max 1
		 :host ewp-blog-address
		 :port "https"))))
    (unless auth
      (error "No credentials for %s in the .authinfo file" ewp-blog-address))
    auth))

(defun ewp-get-posts (blog-xmlrpc user-name password blog-id posts)
  "Retrieves list of posts from the weblog system. Uses wp.getPages."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPosts"
                       blog-id
                       user-name
                       password
		       `(("number" . ,posts))
		       ["post_title" "post_date" "post_status"]))

(defun ewp-select-post ()
  "Edit the post under point."
  (interactive)
  )

(defun ewp-sort-date (e1 e2)
  (time-less-p (caddr (assoc "post_date" e1))
	       (caddr (assoc "post_date" e2))))

(provide 'ewp)

;;; ewp.el ends here
