;;; ewp.el --- Edit Wordpress -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

;; ewp is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; ewp is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;; Install

;; xml-rpc from https://github.com/larsmagne/xml-rpc-el

;; (The "official" version has problem with non-ASCII text on Emacs
;; versions 25 and up.)

;; metaweblog from https://github.com/org2blog/metaweblog

;; # apt install exiftool
;; if you want images to be properly rotated.

;; (setq ewp-blog-address "my.blog.com")
;;
;; This means that the XML-RPC endpoint is on
;; "https://my.blog.com/xmlrpc.php".
;;
;; `M-x ewp' to get started.

;;; Code:

(require 'cl)
(require 'metaweblog)

(defvar ewp-blog-address nil
  "The name/address of the blog, like my.example.blog.")

(defvar ewp-blog-id 1
  "The Wordpress ID of the blog, which is usually 1.")

(defvar ewp-send-hook nil
  "Hook functions run after posting/editing a blog article.")

(defvar ewp-image-width 840
  "What width to tell Wordpress to resize images to when displaying on the blog.")

(defvar ewp-post)

(defvar ewp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "e" 'ewp-select-post)
    (define-key map "n" 'ewp-new-post)
    (define-key map "g" 'ewp)
    (define-key map "\r" 'ewp-browse)
    map))

(define-derived-mode ewp-list-mode special-mode "ewp"
  "Major mode for listing Wordpress posts.

All normal editing commands are switched off.
\\<ewp-mode-map>"
  (buffer-disable-undo)
  (set-face-attribute 'variable-pitch nil :height 100)
  (setq truncate-lines t
	buffer-read-only t))

(defun ewp ()
  "List all the posts on the blog."
  (interactive)
  (switch-to-buffer (format "*%s posts*" ewp-blog-address))
  (let* ((auth (ewp-auth))
	 (inhibit-read-only t))
    (erase-buffer)
    (ewp-list-mode)
    (dolist (post
	     (ewp-get-posts
	      (format "https://%s/xmlrpc.php" ewp-blog-address)
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
		 :port "https"
		 :require '(:user :secret)
		 :create t))))
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
	 (post (metaweblog-get-post
		(format "https://%s/xmlrpc.php" ewp-blog-address)
		(getf auth :user) (funcall (getf auth :secret))
		(cdr (assoc "post_id" data)))))
    (switch-to-buffer (format "*%s edit*" (cdr (assoc "post_id" data))))
    (erase-buffer)
    (ewp-edit-mode)
    (insert "Title: " (cdr (assoc "title" post)) "\n")
    (insert "Categories: " (mapconcat 'identity (cdr (assoc "categories" post))
				      ",")
	    "\n")
    (insert "Status: " (cdr (assoc "post_status" post)) "\n")
    (insert "\n")
    (insert (cdr (assoc "description" post)))
    (goto-char (point-min))
    (ewp-update-images)
    (ewp-save-buffer (cdr (assoc "post_id" data)))
    (setq-local ewp-post post)))

(defun ewp-update-images ()
  (save-excursion
    (let ((urls nil))
      (while (re-search-forward "<img.*src=.\\([^\"]+\\)" nil t)
	(push (match-string 1) urls))
      (ewp-update-image (nreverse urls) (current-buffer)))))

(defun ewp-url-retrieve (url callback)
  (let ((cache (url-cache-create-filename url)))
    (if (file-exists-p cache)
	(with-current-buffer (generate-new-buffer " *ewp url cache*")
	  (erase-buffer)
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally cache)
	  (funcall callback nil))
      (url-retrieve url callback nil t t))))

(defun ewp-update-image (urls buffer)
  (when urls
    (let ((url (pop urls)))
      (ewp-url-retrieve
       url
       (lambda (_)
	 (goto-char (point-min))
	 (let ((buf (current-buffer)))
	   (when (search-forward "\n\n" nil t)
	     (url-store-in-cache)
	     (let ((image (buffer-substring (point) (point-max))))
	       (when (buffer-live-p buffer)
		 (with-current-buffer buffer
		   (save-excursion
		     (goto-char (point-min))
		     (when (re-search-forward
			    (format "<a .*<img.*%s.*</a>" (regexp-quote url))
			    nil t)
		       (with-silent-modifications
			 (put-text-property
			  (match-beginning 0) (match-end 0)
			  'display
			  (create-image image 'imagemagick t
					:max-width 400)))))))))
	   (kill-buffer buf))
	 (when (buffer-live-p buffer)
	   (ewp-update-image urls buffer)))))))

(defun ewp-sort-date (e1 e2)
  (time-less-p (caddr (assoc "post_date" e1))
	       (caddr (assoc "post_date" e2))))

(defvar ewp-edit-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'ewp-update-post)
    map))

(define-derived-mode ewp-edit-mode text-mode "ewp"
  "Major mode for editing Wordpress posts.
\\<ewp-mode-map>"
  (setq-local word-wrap t)
  (setq-local normal-auto-fill-function 'ignore))

(defun ewp-update-post ()
  "Update the post in the current buffer on Wordpress."
  (interactive)
  (run-hooks 'ewp-send-hook)
  (save-buffer)
  (ewp-transform-and-upload)
  (save-excursion
    (goto-char (point-min))
    (let ((headers nil)
	  (post (or (copy-list ewp-post)
		    `(("title")
		      ("description")
		      ("categories"))))
	  (auth (ewp-auth)))
      (while (looking-at "\\([^\n:]+\\): \\(.*\\)")
	(push (cons (match-string 1) (match-string 2)) headers)
	(forward-line 1))
      (forward-line 1)
      (setcdr (assoc "description" post)
	      (buffer-substring (point) (point-max)))
      (setcdr (assoc "title" post) (cdr (assoc "Title" headers)))
      (setcdr (assoc "categories" post)
	      (split-string (cdr (assoc "Categories" headers)) ","))
      (nconc post
	     (list (cons "date"
			 (format-time-string
			  "%Y%m%dT%H:%M:%S"
			  (caddr (assoc "dateCreated" post))
			  ;; When posting new posts you have to use
			  ;; the Californian time zone?  Because
			  ;; Wordpress.com is in San Francisco?
			  (and (caddr (assoc "dateCreated" post))
			       "America/Los_Angeles")))))
      (funcall
       (if ewp-post
	   'metaweblog-edit-post
	 'metaweblog-new-post)
       (format "https://%s/xmlrpc.php" ewp-blog-address)
       (getf auth :user) (funcall (getf auth :secret))
       (format "%s" (cdr (assoc "postid" post)))
       post
       ;; Publish if already published.
       (equal (cdr (assoc "Status" headers)) "publish"))
      (set-buffer-modified-p nil)
      (message "%s the post"
	       (if ewp-post
		   "Edited"
		 "Posted"))
      (bury-buffer))))

(defun ewp-new-post ()
  "Start editing a new post."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Wordpress*"))
  (ewp-edit-mode)
  (setq ewp-post nil)
  (insert "Title: \nCategories: \nStatus: draft\n\n")
  (goto-char (point-min))
  (end-of-line)
  (ewp-save-buffer))

(defun ewp-upload-media (file &optional image)
  (let ((auth (ewp-auth)))
    (metaweblog-upload-file
     (format "https://%s/xmlrpc.php" ewp-blog-address)
     (getf auth :user) (funcall (getf auth :secret))
     (format "%s" ewp-blog-id)
     `(("name" . ,(file-name-nondirectory file))
       ("type" . ,(mailcap-file-name-to-mime-type file))
       ("bits" . ,(with-temp-buffer
		    (set-buffer-multibyte nil)
		    (ewp-possibly-rotate-image file image)
		    (base64-encode-region (point-min) (point-max))
		    (buffer-string)))))))

(defun ewp-transform-and-upload ()
  "Look for local <img> and upload images from those to Wordpress."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<img.*src=\"\\([^\"]+\\).*>" nil t)
      (let ((file (match-string 1))
	    (start (match-beginning 0))
	    (end (match-end 0)))
	;; Local file.
	(when (null (url-type (url-generic-parse-url file)))
	  (let* ((result (ewp-upload-media
			  file (get-text-property start 'display)))
		 (size (image-size (create-image file) t))
		 (url (cdr (assoc "url" result)))
		 factor)
	    (when (> (car size) ewp-image-width)
	      (setq factor (/ (* ewp-image-width 1.0) (car size))))
	    (when url
	      (delete-region start end)
	      (goto-char start)
	      (insert
	       (format
		"<a href=\"%s\"><img src=\"%s%s\" alt=\"\" width=\"%d\" height=\"%d\" class=\"alignnone size-full wp-image-%s\" /></a>"
		url url
		(if factor
		    (format "?w=%d" ewp-image-width)
		  "")
		(if factor
		    ewp-image-width
		  (car size))
		(if factor
		    (* (cdr size) ewp-image-width)
		  (cdr size))
		(cdr (assoc "id" result)))))))))))

(defun ewp-possibly-rotate-image (file-name image)
  (if (or (null image)
	  (not (consp image))
	  (not (eq (car image) 'image))
	  (not (image-property image :rotation))
	  (not (executable-find "exiftool")))
      (insert-file-contents-literally file-name)
    (call-process "exiftool"
		  file-name
		  (list (current-buffer) nil)
		  nil
		  (format "-Orientation#=%d"
			  (cl-case (truncate
				    (image-property image :rotation))
			    (0 0)
			    (90 6)
			    (180 3)
			    (270 8)
			    (otherwise 0)))
		  "-o" "-"
		  "-")))

(defun ewp-remove-image-thumbnails ()
  "Remove thumbnails."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((props (get-text-property (point) 'display)))
	(when (and (consp props)
		   (eq (car props) 'image))
	  (put-text-property (point) (1+ (point)) 'display nil)))
      (forward-char 1))))

(defun ewp-browse ()
  "Examine the blog post under point."
  (interactive)
  (eww (cdr (assoc "short_url" (get-text-property (point) 'data)))))

(defun ewp-make-post-with-image-files (files)
  "Make a post containing the current dired-marked image files."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (ewp-new-post)
  (end-of-buffer)
  (dolist (file files)
    (insert-image (create-image file 'imagemagick nil
				:max-width 700)
		  (format "<img src=%S>" file))
    (insert "\n\n"))
  (goto-char (point-min))
  (end-of-line))

(defun ewp-save-buffer (&optional post-id)
  "Associate the current buffer with a file."
  (let ((file (format "~/.emacs.d/ewp/%s/%s"
		      ewp-blog-address
		      (or post-id (make-temp-name "ewp-")))))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (write-region (point-min) (point-max) file nil t)))
    
(provide 'ewp)

;;; ewp.el ends here
