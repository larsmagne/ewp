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

(define-derived-mode ewp-list-mode special-mode "ewp"
  "Major mode for listing Wordpress posts.

All normal editing commands are switched off.
\\<ewp-mode-map>"
  (buffer-disable-undo)
  (set-face-attribute 'variable-pitch nil :height 100)
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
    (dolist (post
	     (ewp-get-posts (format "https://%s/xmlrpc.php" ewp-blog-address)
			    (getf auth :user) (funcall (getf auth :secret))
			    ewp-blog-id 100))
      (ewp-print-entry post))
    (goto-char (point-min))))

(defun ewp-limit-string (string length)
  (if (< (length string) length)
      string
    (substring string 0 length)))

(defun ewp-print-entry (post)
  "Insert a Wordpress entry at point."
  (insert
   (propertize
    (format
     "%s %s%s%s%s%s\n"
     (propertize
      (format-time-string "%Y-%m-%d" (caddr (assoc "post_date" post)))
      'face 'variable-pitch)
     (propertize 
      (ewp-limit-string (cdr (assoc "post_status" post)) 10)
      'face '(variable-pitch :foreground "#a0a0a0"))
     (propertize " " 'display '(space :align-to 20))
     (propertize
      (ewp-limit-string
       (mapconcat
	'identity
	(loop for term in (cdr (assoc "terms" post))
	      when (equal (cdr (assoc "taxonomy" term)) "category")
	      collect (cdr (assoc "name" term)))
	",")
       20)
      'face '(variable-pitch :foreground "#b0b0b0"))
     (propertize " " 'display '(space :align-to 40))
     (propertize
      (cdr (assoc "post_title" post))
      'face 'variable-pitch))
    'data post)))

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
		       ["post_title" "post_date" "post_status" "terms"]))

(defun ewp-select-post ()
  "Edit the post under point."
  (interactive)
  (let* ((data (get-text-property (point) 'data))
	 (auth (ewp-auth))
	 (post (ewp-get-post
		(format "https://%s/xmlrpc.php" ewp-blog-address)
		(getf auth :user) (funcall (getf auth :secret))
		(cdr (assoc "post_id" data)))))
    (switch-to-buffer (format "*%s edit*" (cdr (assoc "post_id" data))))
    (erase-buffer)
    (ewp-edit-mode)
    (insert "Title: " (cdr (assoc "title" post)) "\n")
    (insert "\n")
    (insert (cdr (assoc "description" post)))
    (goto-char (point-min))
    (setq-local ewp-data data)))

(defun ewp-sort-date (e1 e2)
  (time-less-p (caddr (assoc "post_date" e1))
	       (caddr (assoc "post_date" e2))))

(defun ewp-get-post (blog-xmlrpc user-name password post-id)
  "Retrieves a post from the weblog. POST-ID is the id of the post
which is to be returned.  Can be used with pages as well."
  (xml-rpc-method-call blog-xmlrpc
                       "metaWeblog.getPost"
                       post-id
                       user-name
                       password))

(defvar ewp-list-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map html-mode-map)
    (define-key map "\C-c\C-c" 'ewp-update-post)
    map))

(define-derived-mode ewp-edit-mode html-mode "ewp"
  "Major mode for editing Wordpress posts.

All normal editing commands are switched off.
\\<ewp-mode-map>")

(provide 'ewp)

;;; ewp.el ends here
