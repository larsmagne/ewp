;;; ewp.el --- Manage and Edit Wordpress Posts -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress, blogs
;; Package: ewp
;; Version: 1.0
;; Package-Requires: ((emacs "29.0.59") (xml-rpc "1.6.15"))

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

;; xml-rpc from https://github.com/hexmode/xml-rpc-el

;; # apt install exiftool
;; if you want images to be properly rotated.

;; `M-x ewp' to get started.

;; To handle image and videos you should have ImageMagick and ffmpeg
;; installed.

;; If you have several blogs you can list them all:

;; (setq ewp-blog-addresses '("my.example.com" "other.foo.bar"))


;;; Code:

(require 'cl-lib)
(require 'mm-url)
(require 'dired)
(require 'eww)
(require 'xml-rpc)
(require 'sgml-mode)
(require 'vtable)
(require 'image-crop)
(require 'exif)
(require 'filenotify)
(require 'iso8601)

(defvar ewp-blog-address nil
  "The name/address of the blog, like my.example.blog.")

(defvar ewp-blog-addresses nil
  "A list of name/address of several blogs.")

(defvar ewp-blog-id 1
  "The Wordpress ID of the blog, which is usually 1.")

(defvar ewp-edit-hook nil
  "Hook functions run when starting to edit a post.")

(defvar ewp-send-hook nil
  "Hook functions run after posting/editing a blog article.")

(defvar ewp-set-featured-image nil
  "Whether to set featured image automatically.")

(defvar ewp-image-width 840
  "What width to tell Wordpress to resize images to when displaying on the blog.")

(defvar ewp-floating-image-width 300
  "What size to use for images that float.")

(defvar ewp-embed-smaller-images nil
  "If non-nil, should be a regexp to match blog name to use -1024x768 in the <img>.")

(defvar ewp-display-width nil
  "Max width of imaged when editing.
If nil, use the frame width.")

(defvar ewp-exif-rotate nil
  "If non-nil, rotate images by updating exif data.
If nil, rotate the images \"physically\".")

(defvar ewp-html-tags
  '("b" "blockquote" "body" "div" "em" "h1" "h2" "h3" "h4" "h5" "h6"
    "i" "img" "ul" "li" "ol" "pre" "span" "table" "td" "tr" "u")
  "A list of HTML tags that you might want to complete over.")

(defvar ewp-address-map nil
  "Mapping from external to admin addresses.
This is useful if, for instance, the blog is behind Cloudflare, but
you want the xmlrpc stuff to go directly to the blog.")

(defvar ewp-upload-ssh-addresses nil
  "Directory to upload large video files to.
This is a list where every entry is on the form:

 (\"example.blog\"
  (* 20 1024 1024)
  \"/ssh:foo@example.blog:/var/www/html/video/\"
  \"/video/\")

The first element is the blog name -- you can have different
upload addresses for different blogs.
Video files larger than the second element will be uploaded via ssh.
The third element is the Tramp address to upload to.
The fourth element is the URL prefix to be used for the resulting URL.")

(defvar ewp-screenshot-function #'ewp-screenshot-gnome
  "Function called to insert a screenshot in the current buffer.
Possible functions are `ewp-screenshot-imagemagick' and
`ewp-screenshot-gnome'.")

(defvar ewp-watch-directory nil
  "Directory to automatically insert images from.")

(defvar ewp--timers nil)
(defvar ewp--deletable-files nil)
(defvar ewp--notification-descriptors nil)

(defvar ewp-post)
(defvar ewp-address)
(defvar ewp-categories)
(defvar ewp-comment)
(defvar ewp-edit)
(defvar ewp-marks)
(defvar ewp-deleted-comments)
(defvar ewp-deleted-posts)

(defvar-keymap ewp-list-mode-map
  "e" #'ewp-select-post
  "p" #'ewp-preview
  "M" #'ewp-list-media
  "n" #'ewp-new-post
  "N" #'ewp-new-page
  "g" #'ewp-blog
  "s" #'ewp-list-posts-with-status
  "RET" #'ewp-browse
  "w" #'ewp-copy-link
  "d" #'ewp-trash-post
  ;;"u" #'ewp-undelete-post
  "c" #'ewp-make-comment
  "C" #'ewp-list-comments
  "A" #'ewp-list-posts-with-category
  "S" #'ewp-search-posts
  ">" #'ewp-load-more-posts)

(define-derived-mode ewp-list-mode special-mode "ewp"
  "Major mode for listing Wordpress posts.

All normal editing commands are switched off."
  (setq truncate-lines t)
  (setq-local ewp-deleted-posts nil))

(defun ewp--display-width ()
  (or ewp-display-width
      (truncate (* (frame-pixel-width) 0.9))))

(defun ewp--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(defmacro ewp-save-excursion (&rest body)
  (declare (indent 0))
  (let ((location (gensym)))
    `(let ((,location (ewp-get-location)))
       (prog1
	   ,@body
	 (ewp-restore-location ,location)))))

(defun ewp-get-location ()
  (list :point (point)
	:data (get-text-property (point) 'vtable-object)
	:size (buffer-size)
	:line (count-lines (point-min) (point))))

(defun ewp-restore-location (loc)
  (cond
   ;; We started with an empty buffer, so place point after the header
   ;; line.
   ((zerop (cl-getf loc :size))
    (goto-char (point-min))
    (unless (eobp)
      (forward-line 1)))
   ;; Find a specific element we were on.
   ((cl-getf loc :data)
    (goto-char (point-min))
    (while (and (not (eobp))
		(not (ewp-item-equal
		      (cl-getf loc :data)
		      (get-text-property (point) 'vtable-object))))
      (forward-line 1)))
   ;; Go to the same numeric line.
   (t
    (goto-char (point-min))
    (forward-line (cl-getf loc :line)))))

(defun ewp-item-equal (e1 e2)
  (cl-loop for name in '("page_id" "post_id" "comment_id" "attachment_id")
	   for v1 = (cdr (assoc name e1))
	   for v2 = (cdr (assoc name e2))
	   ;; We found the right type to compare.
	   if (and v1 v2)
	   return (equal v1 v2)))

(defun ewp-list-posts-with-status (status)
  "List posts with a specific status."
  (interactive (list (completing-read "List status: "
				      '("draft" "publish" "schedule"))))
  (ewp-blog ewp-address nil status))

(defun ewp-list-posts-with-category (category)
  "List posts with from a specific category."
  (interactive (list (completing-read "Category: " (ewp-categories))))
  (ewp-blog ewp-address nil nil category))

(defun ewp-load-more-posts (&optional all)
  "Load more posts from the blog.
If ALL (the prefix), load all the posts in the blog."
  (interactive "P")
  (ewp-blog ewp-address (length (ewp-current-data)) nil nil all))

(defun ewp-get-pagelist (url user password blog-id)
  (xml-rpc-method-call url "wp.getPageList" blog-id user password))

(defun ewp-search-posts (string)
  "Search for posts that match a string."
  (interactive "sSearch for: ")
  (let ((address ewp-address)
	(posts (ewp-call 'ewp-get-posts ewp-address
			 300 nil nil nil string)))
    (unless posts
      (error "No posts matched %S" string))
    (switch-to-buffer (format "*%s search*" address))
    (ewp-save-excursion
      (let ((inhibit-read-only t))
	(erase-buffer)
	(ewp-list-mode)
	(setq-local ewp-address address)
	(ewp--prepare-post-list posts)))))

(defun ewp-blog (&optional address start-at status category all)
  "List the posts on the blog."
  (interactive (list (cond
		      ((and (boundp 'ewp-address)
			    ewp-address)
		       ewp-address)
		      (ewp-blog-address)
		      (t
		       (read-string "Blog address (eg. my.example.com): "
				    nil 'ewp-history)))))
  (switch-to-buffer (format "*%s posts*" address))
  (ewp-save-excursion
    (let ((inhibit-read-only t)
	  (data (ewp-current-data)))
      (erase-buffer)
      (ewp-list-mode)
      (setq-local ewp-address address)
      (dolist (post (ewp-call 'ewp-get-posts address
			      (if all 30000 300)
			      start-at status))
	(when (or (null category)
		  (member category (ewp--categories post)))
	  (let* ((id (cdr (assoc "post_id" post)))
		 (old (seq-find (lambda (e)
				  (equal (cdr (assoc "post_id" e)) id))
				data)))
	    ;; If we have an old entry, then remove it.
	    (when old
	      (setq data (delq old data)))
	    (push post data))))
      ;; Also include Pages.
      (when (and (not start-at)
		 (not status))
	(dolist (post (ewp-call 'ewp-get-pagelist address))
	  (let* ((id (cdr (assoc "page_id" post)))
		 (old (seq-find (lambda (e)
				  (equal (cdr (assoc "page_id" e)) id))
				data)))
	    ;; If we have an old entry, then remove it.
	    (when old
	      (setq data (delq old data)))
	    (push post data))))
      (ewp--prepare-post-list data))))

(defun ewp--prepare-post-list (data)
  ;; We're sorting by date, but we want the newest post per date
  ;; to be first.
  (setq data
	(sort data
	      (lambda (p1 p2)
		(> (ewp--post-date p1) (ewp--post-date p2)))))
  (make-vtable
   :columns '((:name "Date" :width 10 :primary descend)
	      (:name "Status" :width 10)
	      (:name "Categories" :width 15)
	      "Title")
   :objects data
   :getter
   (lambda (post column vtable)
     (let* ((prefix (if (assoc "page_title" post)
			"page"
		      "post"))
	    (date (or (caddr (assoc "post_date" post))
		      (caddr (assoc "date_created_gmt" post))))
	    (status (cdr (assoc (format "%s_status" prefix) post))))
       (when (and (equal status "publish")
		  (time-less-p (current-time) date))
	 (setq status "schedule"))
       (pcase (vtable-column vtable column)
	 ("Date" (format-time-string "%Y-%m-%d" date))
	 ("Status" (propertize (or status "")
			       'face '(:foreground "#a0a0a0")))
	 ("Categories"
	  (propertize
	   (mapconcat 'mm-url-decode-entities-string
		      (ewp--categories post) ",")
	   'face '(:foreground "#b0b0b0")))
	 ("Title"
	  (mm-url-decode-entities-string
	   (or (cdr (assoc (format "%s_title" prefix) post)) ""))))))
   :keymap ewp-list-mode-map))

(defun ewp--post-date (post)
  (float-time
   (or (caddr (assoc "post_date" post))
       (caddr (assoc "date_created_gmt" post)))))

(defun ewp--categories (post)
  (cl-loop for term in (cdr (assoc "terms" post))
	   when (equal (cdr (assoc "taxonomy" term)) "category")
	   collect (cdr (assoc "name" term))))

(defun ewp-auth (address)
  (let ((auth
         (nth 0 (auth-source-search
		 :max 1
		 :host address
		 :port "https"
		 :require '(:user :secret)
		 :create t))))
    (unless auth
      (error "No credentials for %s in the .authinfo file" address))
    auth))

(defun ewp-get-posts (blog-xmlrpc user-name password blog-id posts
				  &optional offset status
				  fields search)
  "Retrieves list of posts from the weblog system. Uses wp.getPosts."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPosts"
                       blog-id
                       user-name
                       password
		       `(("number" . ,posts)
			 ("offset" . ,(or offset 0))
			 ,@(and status (list `("post_status" . ,status)))
			 ,@(and search (list `("s" . ,search))))
		       (or fields
			   ["post_title" "post_date" "post_status" "terms"
			    "link" "post_name"])))

(defun ewp-xmlrpc-url (address)
  (format "https://%s/xmlrpc.php"
	  (or (cadr (assoc address ewp-address-map))
	      address)))

(defun ewp-get-post-content (address id pagep)
  (let* ((auth (ewp-auth (or address ewp-address)))
	 (post (apply
		#'xml-rpc-method-call
		`(,(ewp-xmlrpc-url (or address ewp-address))
		  ,@(if pagep
			'("wp.getPage" nil)
		      '("metaWeblog.getPost"))
		  ,id ,(cl-getf auth :user)
		  ,(funcall (cl-getf auth :secret))))))
    (cdr (assoc "description" post))))  

(defun ewp-select-post (&optional address id)
  "Edit the post under point."
  (interactive)
  (let* ((data (get-text-property (point) 'vtable-object))
	 (pagep (assoc "page_id" data))
	 (id (or id (if pagep
			(cdr (assoc "page_id" data))
		      (cdr (assoc "post_id" data)))))
	 (auth (ewp-auth (or address ewp-address)))
	 (post (apply
		#'xml-rpc-method-call
		`(,(ewp-xmlrpc-url (or address ewp-address))
		  ,@(if pagep
			'("wp.getPage" nil)
		      '("metaWeblog.getPost"))
		  ,id ,(cl-getf auth :user)
		  ,(funcall (cl-getf auth :secret)))))
	 (date (or (caddr (assoc "post_date" post))
		   (caddr (assoc "date_created_gmt" post))))
	 (status (cdr (assoc (if pagep
				 "page_status"
			       "post_status")
			     post)))
	 (address (or address ewp-address)))
    (switch-to-buffer (format "*%s edit*" id))
    (erase-buffer)
    (ewp-edit-mode)
    (setq-local ewp-address address)
    (insert "Title: " (or (cdr (assoc "title" post)) "") "\n")
    (insert "Categories: " (mapconcat 'identity (cdr (assoc "categories" post))
				      ",")
	    "\n")
    (insert "Status: " status "\n")
    (when (and (equal status "publish")
	       (time-less-p (current-time) date))
      (insert (format-time-string "Schedule: %FT%T\n"
				  (+ (float-time date)
				     (car (current-time-zone))))))
    (insert "\n")
    (insert (or (cdr (assoc "description" post)) ""))
    (goto-char (point-min))
    (ewp-save-buffer id)
    (ewp--hide-links)
    (setq-local ewp-post post)))

(defun ewp-update-images (max)
  (ewp-update-image
   (cl-loop for img in (dom-by-tag (libxml-parse-html-region
				    (point) (point-max))
				   'img)
	    repeat (or max 100)
	    when (dom-attr img 'src)
	    collect (dom-attr img 'src))
   (current-buffer)))

(defun ewp-url-retrieve (url callback)
  (let ((cache (url-cache-create-filename url)))
    (if (file-exists-p cache)
	(with-current-buffer (generate-new-buffer " *ewp url cache*")
	  (erase-buffer)
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally cache)
	  (funcall callback nil))
      (ignore-errors
	(url-retrieve url callback nil t t)))))

(defun ewp-update-image (urls buffer)
  (when urls
    (let ((url (pop urls)))
      (ewp-url-retrieve
       (mm-url-decode-entities-string url)
       (lambda (_)
	 (goto-char (point-min))
	 (let ((buf (current-buffer)))
	   (when (search-forward "\n\n" nil t)
	     (url-store-in-cache)
	     (let ((image (buffer-substring (point) (point-max)))
		   (content-type
		    (save-excursion
		      (save-restriction
			(narrow-to-region (point-min) (point))
			(let ((content-type (mail-fetch-field "content-type")))
			  (and content-type
			       ;; Remove any comments in the type string.
			       (intern
				(replace-regexp-in-string ";.*" "" content-type)
				obarray)))))))
	       (when (buffer-live-p buffer)
		 (with-current-buffer buffer
		   (save-excursion
		     (goto-char (point-min))
		     (when (re-search-forward
			    (format "<a [^\n>]+?> *<img[^\n>]+?%s[^\n>]+> *</a>\\|<img[^\n>]+?%s[^\n>]*>"
				    (regexp-quote url)
				    (regexp-quote url))
			    nil t)
		       (with-silent-modifications
			 (add-text-properties
			  (match-beginning 0) (match-end 0)
			  (list 'display
				(create-image
				 image (ewp--image-type) t
				 :max-width (ewp--display-width)
				 :max-height
				 (truncate (* (frame-pixel-height) 0.8))
				 :format content-type)
				'keymap image-map
				'inhibit-isearch t)))))))))
	   (kill-buffer buf))
	 (when (buffer-live-p buffer)
	   (ewp-update-image urls buffer)))))))

(defun ewp-sort-date (e1 e2)
  (time-less-p (caddr (assoc "post_date" e1))
	       (caddr (assoc "post_date" e2))))

(defvar-keymap ewp-edit-mode-map
  "C-c C-a" #'ewp-yank-with-href
  "C-c C-y" #'ewp-yank-link-with-text
  "C-c C-b" #'ewp-yank-with-blockquote
  "C-c C-c" #'ewp-update-post
  "C-c C-d" #'ewp-download-and-insert-image
  "C-c C-i" #'ewp-insert-img
  "C-c C-M-t" #'ewp-insert-title
  "C-c C-v" #'ewp-insert-video-file
  "C-c C-V" #'ewp-insert-video-url
  "C-c C-l" #'ewp-insert-lyte
  "C-c C-m" #'ewp-yank-html
  "C-c C-n" #'ewp-clean-link
  "C-c C-o" #'ewp-html-quote-region
  "C-c C-p" #'ewp-yank-picture
  "C-c C-q" #'ewp-remove-image-thumbnails
  "C-c C-w" #'ewp-insert-image-thumbnails
  "C-c C-r" #'ewp-tag-region
  "C-c C-s" #'ewp-import-screenshot
  "C-c C-t" #'ewp-insert-tag
  "C-c C-u" #'ewp-unfill-paragraph
  "C-c C-z" #'ewp-schedule
  "C-c C-k" #'ewp-image-crop
  "C-c C-f" #'ewp-float-image
  "C-c C-S-t" #'ewp-trim-image
  "C-c C-j" #'ewp-set-image-width
  "TAB" #'ewp-complete
  "C-c C-$" #'ewp-toggle-thumbnail)

(define-derived-mode ewp-edit-mode text-mode "ewp"
  "Major mode for editing Wordpress posts.
\\<ewp-edit-mode-map>"
  (setq-local word-wrap t)
  (setq-local normal-auto-fill-function 'ignore)
  (setq-local completion-at-point-functions
	      (cons
	       'ewp-complete-status
	       (cons 'ewp-complete-category completion-at-point-functions)))
  (setq-local image-crop-buffer-text-function #'ewp--update-image-crop)
  ;; This preserves the keymap on images, which is somewhat important
  ;; for ewp buffers.
  (setq-local yank-excluded-properties
	      (delete 'keymap yank-excluded-properties))
  (setq-local ewp--timers nil
	      ewp--notification-descriptors nil
	      ewp--deletable-files nil)
  (keymap-set image-map "i c" #'ewp-image-crop)
  (keymap-set image-map "i x" #'ewp-image-cut)
  (keymap-set image-map "i w" #'ewp-image-view)
  (auto-save-mode 1)
  ;; Automatically insert images.
  (when ewp-watch-directory
    (ewp-watch-directory ewp-watch-directory))
  (run-hooks 'ewp-edit-hook))

(defun ewp--update-image-crop (_text image)
  (format "<img src=\"data:%s;base64,%s\">"
	  (image-crop--content-type image)
	  ;; Get a base64 version of the image.
	  (with-temp-buffer
	    (set-buffer-multibyte nil)
	    (insert image)
	    (base64-encode-region (point-min) (point-max) t)
	    (buffer-string))))

(defun ewp--delete-files (files)
  "Delete FILES and if possible, also the parent director{y,ies} of FILES."
  (dolist (file files)
    (when (file-exists-p file)
      (delete-file file))
    (let ((dir (file-name-directory file)))
      (when (and (file-exists-p dir)
		 (directory-empty-p dir))
	(delete-directory dir)))))

(defun ewp-update-post ()
  "Update the post in the current buffer on Wordpress."
  (interactive)
  (mapc #'cancel-timer ewp--timers)
  (setq ewp--timers nil)
  (mapc #'file-notify-rm-watch ewp--notification-descriptors)
  (setq ewp--notification-descriptors nil)
  (run-hooks 'ewp-send-hook)
  (when (buffer-file-name)
    (save-buffer))
  (ewp-transform-and-upload-images ewp-address)
  (ewp-transform-and-upload-videos ewp-address)
  (ewp-transform-and-upload-links ewp-address)
  (if (and (boundp 'ewp-comment)
	   ewp-comment)
      (ewp-send-comment)
    (save-excursion
      (goto-char (point-min))
      (let ((headers nil)
	    (post (copy-sequence (append ewp-post
					 `(("title")
					   ("description")
					   ("categories")))))
	    (pagep (assoc "page_id" ewp-post))
	    (auth (ewp-auth ewp-address)))
	(while (looking-at "\\([^\n:]+\\): \\(.*\\)")
	  (push (cons (match-string 1) (match-string 2)) headers)
	  (forward-line 1))
	(forward-line 1)
	(setcdr (assoc "description" post)
		(buffer-substring (point) (point-max)))
	(setcdr (assoc "title" post) (cdr (assoc "Title" headers)))
	(setcdr (assoc "categories" post)
		(and (cdr (assoc "Categories" headers))
		     (mapcar #'string-trim
			     (split-string (cdr (assoc "Categories" headers))
					   ","))))
	(nconc post
	       (list (cons "date"
			   (ewp-current-time
			    ;; If we're going from "draft" to
			    ;; "publish", then use the current time.
			    ;; In all other cases, preserve the date
			    ;; header.
			    (if (and (equal (cdr (assoc "post_status" ewp-post))
					    "draft")
				     (equal (cdr (assoc "Status" headers))
					    "publish"))
				nil
			      post)
			    (cdr (assoc "Schedule" headers))))))
	(save-excursion
	  (let ((match (text-property-search-forward 'ewp-thumbnail)))
	    (when match
	      (let ((string (buffer-substring (prop-match-beginning match)
					      (prop-match-end match))))
		(and (string-match "wp-image-\\([0-9]+\\)" string)
		     (nconc post (list (cons "thumbnail"
					     (match-string 1 string)))))))))
	(ewp-blog-post
	 pagep
	 ;; The old post/page ID if it's an edit.
	 (or (cdr (assoc "page_id" post))
	     (cdr (assoc "postid" post)))
	 (ewp-xmlrpc-url ewp-address)
	 (cl-getf auth :user)
	 (funcall (cl-getf auth :secret))
	 ewp-blog-id
	 post
	 ;; Publish if already published.
	 (equal (cdr (assoc "Status" headers)) "publish"))
	(set-buffer-modified-p nil)
	(message "%s the post"
		 (if ewp-post
		     "Edited"
		   "Posted"))
	(bury-buffer))))
  ;; Clean up /tmp/ storage.
  (ewp--delete-files ewp--deletable-files)
  (setq ewp--deletable-files nil))

(defun ewp-node (symbol &rest values)
  `(,symbol nil ,@(delq nil values)))

(defun ewp-param (&rest values)
  (ewp-node 'param (apply #'ewp-value values)))

(defun ewp-value (&rest values)
  (apply #'ewp-node 'value values))

(defun ewp-member (&rest values)
  (apply #'ewp-node 'member values))

(defun ewp-blog-post (pagep post-id url user password blog-id post publishp)
  (xml-rpc-xml-to-response
   (xml-rpc-request
    url
    (list
     (ewp-node
      'methodCall
      (ewp-node
       'methodName
       (cond ((and (not pagep) (not post-id)) "metaWeblog.newPost")
	     ((and (not pagep) post-id) "metaWeblog.editPost")
	     ((and pagep (not post-id)) "wp.newPage")
	     ((and pagep post-id) "wp.editPage")))
      (ewp-node
       'params
       ;; We send blog-id if it's a new post/page, but we always send
       ;; it if it's a page.
       (and (or pagep (not post-id))
	    (ewp-param (ewp-node 'string (format "%s" blog-id))))
       ;; Only send post-id if we're editing.
       (and post-id
	    (ewp-param (ewp-node 'string (format "%s" post-id))))
       (ewp-param (ewp-node 'string user))
       (ewp-param (ewp-node 'string password))
       (ewp-param
        (ewp-node
	 'struct
         (ewp-member
          (ewp-node 'name "title")
          (ewp-value (ewp-get "title" post)))
         (ewp-member 
          (ewp-node 'name "description")
          (ewp-value (ewp-get "description" post)))
         (ewp-member
          (ewp-node 'name "dateCreated")
          (ewp-node 'dateTime.iso8601 (ewp-get "date" post)))
         (and (ewp-get "categories" post)
	      (ewp-member
	       (ewp-node 'name "categories")
	       (ewp-value
                (ewp-node 'array
			  (apply #'ewp-node
				 'data
				 (mapcar
				  (lambda (cat)
				    (ewp-value (ewp-node 'string cat)))
				  (ewp-get "categories" post)))))))
	 (let ((thumbnail-id (or (car (ewp-get "new_post_thumbnail" post))
				 (and ewp-set-featured-image
				      (ewp--automatic-featured-image)))))
	   (and thumbnail-id
		(not pagep)
		;; Wordpress doesn't like when we set the thumbnail to
		;; what it already is.
		(not (equal thumbnail-id
			    (ewp-get "wp_post_thumbnail" post)))
		(ewp-member
		 (ewp-node 'name "wp_post_thumbnail")
		 (ewp-value
		  (format "%d" thumbnail-id)))))))
       (ewp-param (ewp-node 'boolean (if publishp "1" "0")))))))))

(defun ewp--automatic-featured-image ()
  (save-excursion
    (goto-char (point-min))
    (when (and (search-forward "<img " nil t)
	       (re-search-forward "wp-image-\\([0-9]+\\)" (pos-eol) t))
      (string-to-number (match-string 1)))))

(defun ewp-external-time (time)
  (format-time-string "%Y%m%dT%H:%M:%S" time))

;; I ... think?  Wordpress uses universal time for the time stamps,
;; but I'm not sure.  They call it "GMT", though.  This code assumes
;; to, anyway.
(defun ewp-current-time (post scheduled)
  (ewp-external-time
   (if (plusp (length scheduled))
       (- (time-convert (encode-time (iso8601-parse scheduled)) 'integer)
	  (car (current-time-zone)))
     (or (caddr (assoc "date_created_gmt" post))
	 (caddr (assoc "dateCreated_gmt" post))
	 ;; Convert to univesal.
	 (- (time-convert (current-time) 'integer)
	    (car (current-time-zone)))))))

(defun ewp-new-post (&optional address buffer
			       title categories status)
  "Start editing a new post."
  (interactive)
  (let ((address (or address ewp-address)))
    (switch-to-buffer (or buffer (generate-new-buffer "*Wordpress Post*")))
    (ewp-edit-mode)
    (setq-local ewp-post nil)
    (setq-local ewp-address address)
    (insert (format "Title: %s\nCategories: %s\nStatus: %s\n\n"
		    (or title "")
		    (or categories "")
		    (or status "draft")))
    (goto-char (point-min))
    (end-of-line)
    (ewp-save-buffer)))

(defun ewp-new-page ()
  "Start editing a new page."
  (interactive)
  (let ((address ewp-address))
    (switch-to-buffer (generate-new-buffer "*Wordpress Page*"))
    (ewp-edit-mode)
    (setq-local ewp-post (list (list "page_id")))
    (setq-local ewp-address address)
    (insert "Title: \nStatus: draft\n\n")
    (goto-char (point-min))
    (end-of-line)
    (ewp-save-buffer)))

(defun ewp--upload-file (address name type bits &optional date)
  (let ((auth (ewp-auth address)))
    (xml-rpc-xml-to-response
     (xml-rpc-request
      (ewp-xmlrpc-url address)
      (list
       (ewp-node
	'methodCall
        (ewp-node 'methodName "metaWeblog.newMediaObject")
	(ewp-node
	 'params
	 (ewp-param (ewp-node 'string (format "%s" ewp-blog-id)))
	 (ewp-param (ewp-node 'string (cl-getf auth :user)))
	 (ewp-param (ewp-node 'string (funcall (cl-getf auth :secret))))
         (ewp-param
          (ewp-node
	   'struct
           (ewp-member
            (ewp-node 'name "name")
            (ewp-value name))
	   (ewp-member 
	    (ewp-node 'name "bits")
            (ewp-node 'base64 bits))
	   (ewp-member
	    (ewp-node 'name "type")
	    (ewp-value type))
	   (ewp-member
	    (ewp-node 'name "overwrite")
	    (ewp-value "t"))
	   (ewp-member
            (ewp-node 'name "date")
	    (ewp-value (or date ""))))))))))))

(defun ewp-upload-file (address file &optional image image-name)
  (ewp--upload-file
   address
   (or image-name (file-name-nondirectory file))
   (mailcap-file-name-to-mime-type file)
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally file)
     (ewp-possibly-rotate-buffer image)
     (base64-encode-region (point-min) (point-max))
     (buffer-string))))

(defun ewp-current-image ()
  "Return the image under point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "<img.*?src=\"\\([^\"]+\\)"
			     (pos-eol) t)
      (match-string-no-properties 1))))

(defun ewp-set-featured-image ()
  "Set the current post's featured image to the image under point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "wp-image-\\([0-9]+\\)" (pos-eol) t)
	(let ((image-id (match-string 1)))
	  (if (assoc "new_post_thumbnail" ewp-post)
	      (setcdr (assoc "new_post_thumbnail" ewp-post)
		      (string-to-number image-id))
	    (setq ewp-post (append ewp-post
				   (list (list "new_post_thumbnail"
					       (string-to-number image-id))))))
	  (message
	   (format "Featured image will be updated to %s upon `C-c C-c'"
		   image-id)))
      (user-error "No (uploaded) image at point"))))

(defun ewp-open-image-in-browser ()
  "Open the image under point in the secondary browser."
  (interactive)
  (funcall browse-url-secondary-browser-function
	   (concat "file://" (ewp-current-image))))

(defun ewp-transform-and-upload-images (address)
  "Look for local <img> and upload images from those to Wordpress."
  (interactive (list ewp-address))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(<a [^>]+>.*?\\)?\\(<img.*?src=\"\\)" nil t)
      (let* ((link-start (match-beginning 1))
	     (start (match-beginning 2))
	     (file (buffer-substring-no-properties
		    (point) (progn
			      (re-search-forward "\".*?>" nil t)
			      (match-beginning 0))))
	     (end (point))
	     (link-end (and (looking-at "</a>")
			    (match-end 0)))
	     ;; We're avoiding `url-generic-parse-url' and other
	     ;; regepx-based parsers here because data: URLs can be
	     ;; huge and blows up the regexp parser.
	     (type (and (string-match "^[a-z]+:" file)
			(substring file 0 (1- (match-end 0)))))
	     (image (get-text-property start 'display))
	     (floatp (save-excursion
		       (beginning-of-line)
		       (looking-at "<p style=.clear: both;.>")))
	     result size)
	(unless (equal type "https")
	  (redisplay t)
	  (sit-for 0.1))
	(cond
	 ;; Local file.
	 ((null type)
	  (let ((data
		 (with-temp-buffer
		   (set-buffer-multibyte nil)
		   (insert-file-contents-literally file)
		   (ewp-possibly-rotate-buffer image)
		   (base64-encode-region (point-min) (point-max))
		   (buffer-string))))
	    (setq result
		  (ewp--upload-file address
				    (file-name-nondirectory file)
				    (mailcap-file-name-to-mime-type file)
				    data))
	    (setq size (ewp-image-size (create-image
					(with-temp-buffer
					  (set-buffer-multibyte nil)
					  (insert data)
					  (base64-decode-region
					   (point-min) (point-max))
					  (buffer-string))
					(ewp--image-type) t)))))
	 ;; data: URL where the image is in the src bit.
	 ((and (equal type "data")
	       (string-match "^data:\\([^;]+\\);base64," file))
	  (let ((mime-type (match-string 1 file))
		(data (with-temp-buffer
			(set-buffer-multibyte nil)
			(insert (substring-no-properties file))
			(goto-char (point-min))
			(search-forward ",")
			(delete-region (point-min) (point))
			(base64-decode-region (point-min) (point-max))
			(ewp-possibly-rotate-buffer image)
			(base64-encode-region (point-min) (point-max))
			(buffer-string))))
	    (setq result
		  (ewp--upload-file
		   address
		   (format "%s.%s"
			   (format-time-string "%F")
			   (cadr (split-string mime-type "/")))
		   mime-type
		   data))
	    (setq size (ewp-image-size (create-image
					(with-temp-buffer
					  (set-buffer-multibyte nil)
					  (insert data)
					  (base64-decode-region
					   (point-min) (point-max))
					  (buffer-string))
					(ewp--image-type) t)))))
	 ;; We have a normal <img src="http..."> image, but it's been
	 ;; rotated.
	 ((and image
	       (consp image)
	       (eq (car image) 'image)
	       (image-property image :rotation))
	  (let* ((data
		  (with-temp-buffer
		    (set-buffer-multibyte nil)
		    (insert (cl-getf (cdr image) :data))
		    (ewp-possibly-rotate-buffer image)
		    (buffer-string)))
		 (content-type (ewp-content-type data)))
	    (setq result
		  (ewp--upload-file
		   address
		   (format "%s.%s"
			   (format-time-string "%F")
			   (cadr (split-string content-type "/")))
		   content-type
		   (base64-encode-string data)))
	    (setq size (ewp-image-size (create-image data nil t)))
	    ;; Remove the <a> that we slap around images.
	    (when (and link-start
		       link-end)
	      (setq start link-start
		    end link-end)))))

	(when result
	  (let* ((url (cdr (assoc "url" result)))
		 (unscaled-url 
		  ;; Link to the unscaled version of the image.
		  (replace-regexp-in-string
		   "-scaled\\([.][^.]+\\'\\)" "\\1" url))
		 (thumbnailp (get-text-property start 'ewp-thumbnail))
		 (limit-width (if floatp
				  ewp-floating-image-width
				ewp-image-width))
		 factor)
	    (when (> (car size) limit-width)
	      (setq factor (/ (* limit-width 1.0) (car size))))
	    (when url
	      (delete-region start end)
	      (goto-char start)
	      (if (and ewp-embed-smaller-images
		       (string-match ewp-embed-smaller-images ewp-address))
		  (insert
		   (format
		    "<a class=cimage href=%S><img src=%S alt=\"\" wp-image-%s /></a>"
		    unscaled-url
		    (if (> (car size) 768)
			(replace-regexp-in-string "\\([.][a-z]+\\)\\'"
						  (if (> (car size) (cdr size))
						      "-1024x768\\1"
						    "-768x1024\\1")
						  url)
		      url)
		    (cdr (assoc "id" result))))
		(insert
		 (format
		  "<a class=cimage href=%S><img src=%S alt=\"\" width=\"%d\" height=\"%d\" class=\"alignnone size-full wp-image-%s\" /></a>"
		  unscaled-url url
		  (if factor
		      limit-width
		    (car size))
		  (if factor
		      (* (cdr size) factor)
		    (cdr size))
		  (cdr (assoc "id" result)))))
	      ;; Preserve the thumnail designation.
	      (when thumbnailp
		(put-text-property start (point)
				   'ewp-thumbnail thumbnailp)))))))))

(defun ewp--upload-via-ssh (file address)
  (let* (frag
	 (target
	  ;; Check whether the file exists, and if it does, make a new,
	  ;; unique name.
	  (cl-loop with extension = (file-name-extension file)
		   and base = (file-name-sans-extension
			       (file-name-nondirectory file))
		   for i from 0
		   for counter = "" then (format "-%d" i)
		   for fragment = (concat (format-time-string "%Y/%m/")
					  base counter "." extension)
		   for target = (expand-file-name fragment address)
		   while (file-exists-p target)
		   finally (progn
			     (setq frag fragment)
			     (cl-return target)))))
    (unless (file-exists-p (file-name-directory target))
      (make-directory (file-name-directory target) t))
    (copy-file file target)
    frag))

(defun ewp--possibly-upload-via-ssh (file address)
  (when-let ((elem (assoc address ewp-upload-ssh-addresses)))
    (when (> (file-attribute-size (file-attributes file))
	     (nth 1 elem))
      (let ((fragment (ewp--upload-via-ssh file (nth 2 elem))))
	(format "https://%s%s%s" ewp-address (nth 3 elem) fragment)))))

(defun ewp-transform-and-upload-videos (address)
  "Look for local <video ...> and upload mp4s from those to Wordpress."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<video .*?src=\"\\([^\"]+\\)\"" nil t)
      (let* ((file (match-string 1))
	     (start (match-beginning 1))
	     (end (match-end 1))
	     (video-start (match-beginning 0))
	     (url (url-generic-parse-url file))
	     new-url)
	;; Local file; add poster and upload the file
	(when (null (url-type url))
	  ;; If there's already a poster here, don't make a new one.
	  (unless (save-excursion
		    (re-search-forward "poster=\"\\([^\"]+\\)\""
				       (max end (line-end-position)) t))
	    (push (ewp--add-video-poster file) ewp--deletable-files))
	  (when (or (setq new-url (ewp--possibly-upload-via-ssh file address))
		    (when-let* ((result
				 (ewp--upload-file
				  address
				  (file-name-nondirectory file)
				  (mailcap-file-name-to-mime-type file)
				  (with-temp-buffer
				    (set-buffer-multibyte nil)
				    (insert-file-contents-literally file)
				    (base64-encode-region (point-min)
							  (point-max))
				    (buffer-string)))))
		      (setq new-url (cdr (assoc "url" result)))))
	    (delete-region start end)
	    (goto-char start)
	    (insert new-url)))
	;; Check for <video ... poster="">.
	(save-excursion
	  (goto-char video-start)
	  (when (re-search-forward "poster=\"\\([^\"]+\\)\""
				   (max end (line-end-position)) t)
	    (let* ((file (match-string 1))
		   (start (match-beginning 1))
		   (end (match-end 1))
		   (url (url-generic-parse-url file)))
	      ;; Local file; upload it.
	      (when (null (url-type url))
		(when-let* ((result
			     (ewp--upload-file
			      address
			      (file-name-nondirectory file)
			      (mailcap-file-name-to-mime-type file)
			      (with-temp-buffer
				(set-buffer-multibyte nil)
				(insert-file-contents-literally file)
				(base64-encode-region (point-min)
						      (point-max))
				(buffer-string))))
			    (url (cdr (assoc "url" result))))
		  (delete-region start end)
		  (goto-char start)
		  (insert url))))))))))

(defun ewp--add-video-poster (file)
  (when (executable-find "ffmpeg")
    (let ((prefix (ewp--temp-name "poster")))
      (when (zerop
	     (call-process
	      "ffmpeg" nil nil nil
	      "-i" (expand-file-name file)
	      "-r" "30" "-f" "image2"
	      "-frames:v" "1" "-ss" "0"
	      "-vf" "thumbnail,scale=-1:720"
	      (concat prefix "%03d.jpg")))
	(let ((output (concat prefix "001.jpg")))
	  (insert (format " poster=%S " output))
	  output)))))

(defun ewp-transform-and-upload-links (address)
  "Look for external links and create cached screenshots for those."
  (when (executable-find "cutycapt")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<a " nil t)
	(let ((start (match-beginning 0)))
	  (goto-char start)
	  (with-syntax-table sgml-mode-syntax-table
	    (forward-sexp))
	  (let* ((dom (nth 2 (nth 2 (libxml-parse-html-region start (point)))))
		 (url (url-generic-parse-url (dom-attr dom 'href)))
		 (file (ewp--temp-name "ewp" ".png")))
	    ;; Local file.
	    (when (and (not (equal (url-host url) address))
		       (dom-attr dom 'screenshot)
		       (not (dom-attr dom 'onmouseenter)))
	      (message "Capturing %s..." (dom-attr dom 'href))
	      (let ((proc
		     (start-process "capt" nil
				    "cutycapt"
				    "--out-format=png"
				    (format "--url=%s" (dom-attr dom 'href))
				    (format "--out=%s" file)))
		    (time (float-time)))
		(while (and (process-live-p proc)
			    (< (- (float-time) time) 10))
		  (sit-for 0.1))
		(if (process-live-p proc)
		    (progn
		      (delete-process proc)
		      (save-excursion
			(goto-char start)
			(when (looking-at "<a screenshot=true ")
			  (replace-match "<a "))))
		  (when (file-exists-p file)
		    (when-let* ((result
				 (ewp--upload-file
				  address
				  (file-name-nondirectory file)
				  (mailcap-file-name-to-mime-type file)
				  (with-temp-buffer
				    (set-buffer-multibyte nil)
				    (insert-file-contents-literally
				     file)
				    (base64-encode-region (point-min)
							  (point-max))
				    (buffer-string))))
				(image-url (cdr (assoc "url" result))))
		      (delete-region start (point))
		      (dom-remove-attribute dom 'screenshot)
		      (dom-set-attribute dom 'data-cached-time
					 (format-time-string "%FT%T"))
		      (dom-set-attribute dom 'data-cached-image image-url)
		      (dom-set-attribute dom 'onmouseenter "hoverLink(event)")
		      (ewp-print-html dom t)
		      (delete-file file))))))))))))

(defun dom-remove-attribute (node attribute)
  "Remove ATTRIBUTE from NODE."
  (setq node (dom-ensure-node node))
  (when-let ((old (assoc attribute (cadr node))))
    (setcar (cdr node) (delq old (cadr node)))))

(defun ewp-possibly-rotate-buffer (image &optional rotation)
  (when (and image
	     (consp image)
	     (eq (car image) 'image))
    (setq rotation (or rotation (image-property image :rotation)))
    (when rotation
      (let ((content-type (ewp-content-type (buffer-string))))
	(cond
	 ;; We can rotate jpegs losslessly by setting the correct
	 ;; orientation.
	 ((and ewp-exif-rotate
	       (equal content-type "image/jpeg")
	       (executable-find "exiftool"))
	  (call-process-region
	   (point-min) (point-max) "exiftool" t (list (current-buffer) nil) nil
	   (format "-Orientation#=%d"
		   (cl-case (truncate rotation)
		     (0 0)
		     (90 6)
		     (180 3)
		     (270 8)
		     (otherwise 0)))
	   "-o" "-"
	   "-"))
	 ;; Most other image formats have to be reencoded to do
	 ;; rotation.
	 ((executable-find "convert")
	  (call-process-region
	   (point-min) (point-max) "convert" t (list (current-buffer) nil) nil
	   "-rotate" (format "%d" rotation)
	   "-" "-")
	  ;; This is apparently necessary to avoid having Twitter
	  ;; believe that the picture should be rotated again.
	  (when (and (equal content-type "image/jpeg")
		     (executable-find "exiftool"))
	    (call-process-region
	     (point-min) (point-max) "exiftool" t (list (current-buffer) nil) nil
	     "-Orientation#=0"
	     "-CameraOrientation#=0"
	     "-o" "-" "-"))))))
    (when (and (image-property image :width)
	       (executable-find "convert"))
      (call-process-region
       (point-min) (point-max) "convert" t (list (current-buffer) nil) nil
       "-resize" (format "%dx" (image-property image :width))
       "-" "-"))))

(defun ewp-insert-image-thumbnails (&optional max)
  "Insert thumbnails.
If MAX (the numerical prefix), just do that many thumbnails."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (unless max
    (ewp-remove-image-thumbnails))
  (ewp-update-images max)
  (message "Inserting image thumbnails..."))

(defun ewp--hide-links ()
  (save-excursion
    (with-buffer-unmodified-if-unchanged
      ;; Hide <a> links.
      (goto-char (point-min))
      (while (re-search-forward "<a[ \n]" nil t)
	(goto-char (match-beginning 0))
	(let ((start (point)))
	  (with-syntax-table sgml-mode-syntax-table
	    (forward-sexp))
	  ;; Images in WordPress are typically represented as
	  ;; <a ...><img ...></a> so that you can click on them
	  ;; to see a bigger version.  Handle this specially so we only
	  ;; get one symbol for the entire construction instead of three.
	  (if-let ((end
		    (save-excursion
		      (and (looking-at " *<img[ \n]")
			   (progn
			     (with-syntax-table sgml-mode-syntax-table
			       (forward-sexp))
			     (looking-at " *</a>"))
			   (match-end 0)))))
	      (progn
		(goto-char end)
		(ewp--hide-region "🔳" start end))
	    (ewp--hide-region "[" start (point)))))
      ;; Hide </a>.
      (goto-char (point-min))
      (while (re-search-forward "</a>" nil t)
	(unless (get-text-property (match-beginning 0) 'ewp-element)
	  (ewp--hide-region "]" (match-beginning 0) (match-end 0))))
      ;; Hide <img>.
      (goto-char (point-min))
      (while (re-search-forward "<img[ \n]" nil t)
	(unless (get-text-property (match-end 0) 'ewp-element)
	  (goto-char (match-beginning 0))
	  (let ((start (point)))
	    (with-syntax-table sgml-mode-syntax-table
	      (forward-sexp))
	    (ewp--hide-region "⬜" start (point)))))
      ;; Hide <video>.
      (goto-char (point-min))
      (while (re-search-forward "<video[ \n]" nil t)
	(unless (get-text-property (match-beginning 0) 'ewp-element)
	  (goto-char (match-beginning 0))
	  (let ((start (point)))
	    (search-forward "</video>")
	    (ewp--hide-region "🎥" start (point))))))))

(defvar ewp--element-id 0)

(defun ewp--hide-region (string start end)
  (put-text-property start end 'ewp-element (cl-incf ewp--element-id))
  (let ((st (propertize string 'face 'font-lock-keyword-face)))
    (put-text-property start end 'ewp-display st)
    (put-text-property start end 'display st)
    (put-text-property (1- end) end 'rear-nonsticky t)))

(defun ewp--toggle-element-display ()
  (let ((end (prop-match-end (text-property-search-forward 'ewp-element)))
	(start (prop-match-beginning
		(text-property-search-backward 'ewp-element))))
    (with-buffer-unmodified-if-unchanged
      (cond
       ((stringp (get-text-property (point) 'display))
	(put-text-property start end 'display nil)
	;; See if we have an image to expand.
	(when-let* ((dom (libxml-parse-html-region start end))
		    (img (dom-by-tag dom 'img)))
	  (ewp-update-image (list (dom-attr img 'src))
			    (current-buffer))))
       ((imagep (get-text-property (point) 'display))
	(put-text-property start end 'display nil))
       (t
	(put-text-property start end 'display
			   (get-text-property start 'ewp-display)))))))

(defun ewp-remove-image-thumbnails ()
  "Remove thumbnails and display hidden HTML."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while-let ((props (text-property-search-forward 'display)))
	(when (or (imagep (prop-match-value props))
		  (stringp (prop-match-value props)))
	  (put-text-property (prop-match-beginning props)
			     (prop-match-end props)
			     'display nil))))))

(defun ewp-browse ()
  "Display the blog post under point with `eww'."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No post under point"))
    (eww (or (cdr (assoc "short_url" data))
	     (cdr (assoc "link" data))))))

(defun ewp-preview ()
  "Preview the blog post under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No post under point"))
    (funcall browse-url-secondary-browser-function
	     (format "https://%s/?p=%s&preview=true"
		     ewp-address
		     (cdr (assoc "post_id" data))))))

(defun ewp-make-post-with-image-files (files)
  "Make a post containing the current dired-marked image files."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (ewp-new-post)
  (goto-char (point-max))
  (dolist (file files)
    (insert-image (create-image file (ewp--image-type) nil
				:max-width (ewp--display-width))
		  (format "<img src=%S>" file))
    (insert "\n\n"))
  (goto-char (point-min))
  (end-of-line))

(defun ewp-save-buffer (&optional post-id)
  "Associate the current buffer with a file."
  (let ((file (format "~/.emacs.d/ewp/%s/%s"
		      ewp-address
		      (or post-id (make-temp-name "ewp-")))))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (write-region (point-min) (point-max) file nil t)))

(defun ewp ()
  "List all blogs you have.
Uses `ewp-blog-addresses'."
  (interactive)
  (switch-to-buffer "*ewp*")
  (ewp-list-blogs-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :columns '("Blog")
     :objects ewp-blog-addresses
     :actions '("RET" ewp-blog)
     :keymap (define-keymap
	       "g" #'ewp))))

(define-derived-mode ewp-list-blogs-mode special-mode "ewp"
  "Major mode for listing Wordpress blogs.

All normal editing commands are switched off.
\\<ewp-list-blogs-mode-map>"
  (setq truncate-lines t))

(defun ewp-complete ()
  "Complete categories in that header."
  (interactive)
  (cond
   ((let ((completion-fail-discreetly t))
      (completion-at-point))
    ;; Completion was performed; nothing else to do.
    nil)
   ((get-text-property (point) 'ewp-element)
    (ewp--toggle-element-display))
   (t
    (indent-relative))))

(defun ewp-complete-category ()
  (and (save-excursion
	 (beginning-of-line)
	 (looking-at "Categories: "))
       (lambda ()
	 (let ((categories (mapcar #'car (ewp-categories)))
	       (b (save-excursion
		    (save-restriction
		      (narrow-to-region
		       (save-excursion
			 (beginning-of-line)
			 (skip-chars-forward "^:")
			 (1+ (point)))
		       (point))
		      (skip-chars-backward "^, \t\n") (point))))
	       (e (progn (skip-chars-forward "^,\t\n ") (point)))
	       (completion-ignore-case t))
	   (completion-in-region b e categories)
	   'completion-attempted))))

(defun ewp-complete-status ()
  (and (save-excursion
	 (beginning-of-line)
	 (looking-at "Status: "))
       (lambda ()
	 (let ((statuses '("draft" "publish"))
	       (b (save-excursion
		    (beginning-of-line)
		    (search-forward ": ")))
	       (completion-ignore-case t))
	   (completion-in-region b (line-end-position) statuses)
	   'completion-attempted))))

(defun ewp-get-categories (address)
  (let ((auth (ewp-auth address)))
    (xml-rpc-method-call (ewp-xmlrpc-url address)
                       "metaWeblog.getCategories"
                       (format "%s" ewp-blog-id)
		       (cl-getf auth :user) (funcall (cl-getf auth :secret)))))

(defun ewp-categories ()
  (if (boundp 'ewp-categories)
      ewp-categories
    (setq-local ewp-categories
		(cl-loop for elem in (ewp-get-categories ewp-address)
			 collect (cons (cdr (assoc "categoryName" elem))
				       (cdr (assoc "categoryId" elem)))))))

(defun ewp-yank-with-href (&optional surround)
  "Yank the current kill ring item as an <a href>.
If SURROUND (the prefix interactively), put the <a ...></a>
around the text between mark and point."
  (interactive "P")
  (let ((link (format "<a screenshot=true href=%S>"
		      (substring-no-properties (current-kill 0)))))
    (if surround
	(let ((point (point-marker)))
	  (goto-char (mark))
	  (insert link)
	  (goto-char point)
	  (insert "</a>")
	  (set-marker point nil))
      (set-mark (point))
      (insert link "</a>")
      (forward-char -4))))

(defun ewp-insert-lyte ()
  "Insert a [lyte] tag."
  (interactive)
  (ensure-empty-lines 1)
  (insert (format "[lyte id='%s']\n\n"
		  (substring-no-properties (current-kill 0)))))

(defun ewp-yank-with-blockquote (&optional clipboard)
  "Yank the current kill ring item as a <blockquote>.
If given a prefix, yank from the clipboard."
  (interactive "P")
  (set-mark (point))
  (when-let ((url (x-get-selection-internal 'PRIMARY 'text/x-moz-url-priv)))
    (insert (format "<a screenshot=true href=%S></a>:\n\n"
		    (ewp-decode-text-selection url))))
  (insert "<blockquote>")
  (if clipboard
      (insert (string-trim
	       (decode-coding-string (x-get-selection-internal
				      'CLIPBOARD 'text/plain\;charset=utf-8)
				     'utf-8)))
    (insert (string-trim (substring-no-properties (current-kill 0)))))
  (insert "</blockquote>\n\n"))

(defun ewp-yank-link-with-text ()
  "Yank the current kill ring item as <a href=URL>TEXT</a>."
  (interactive)
  (set-mark (point))
  (let ((url (x-get-selection-internal 'PRIMARY 'text/x-moz-url-priv))
	(text (current-kill 0)))
    (unless url
      (error "No URL in the current kill"))
    (insert (format "<a screenshot=true href=%S>%s</a>"
		    (ewp-decode-text-selection url)
		    (string-trim text)))))

(defun ewp-insert-img (file)
  "Prompt for a file and insert an <img>."
  (interactive "fImage file: ")
  (insert-image (create-image
		 file (ewp--image-type) nil
		 :max-width (truncate (* (frame-pixel-width) 0.8))
		 :max-height (truncate (* (frame-pixel-height) 0.5))
		 :scale 1)
		(format "<img src=%S>" file))
  (insert "\n\n"))

(defun ewp-insert-title (title)
  "Prompt for a title and insert it in the closest <img>."
  (interactive (list (read-string "Title: " (ewp--current-title))))
  (save-excursion
    (ewp--current-title t)
    (search-forward "<img" (pos-eol))
    (insert (apply #'propertize (format " title=%S" title)
		   (text-properties-at (match-beginning 0))))))

(defun ewp--current-title (&optional delete)
  (save-excursion
  (beginning-of-line)
  (if (not (search-forward "<img" (pos-eol) t))
      (error "No <img> on the current line")
    (goto-char (match-beginning 0))
    (let ((start (point))
	  end)
      (with-syntax-table sgml-mode-syntax-table
	(forward-sexp))
      (setq end (point))
      (goto-char start)
      (when (re-search-forward " +title=\"" end t)
	(let ((tstart (match-beginning 0))
	      (tend (match-end 0)))
	  (forward-char -1)
	  (with-syntax-table sgml-mode-syntax-table
	    (forward-sexp))
	  (if delete
	      (delete-region tstart (point))
	    (buffer-substring-no-properties tend (1- (point))))))))))

(defun ewp-insert-video-file (file &optional rescale)
  "Prompt for a file and insert a <video> tag.
If RESCALE (interactively, the prefix, non-interactively the
width), rescale and convert the file to mp4."
  (interactive "fVideo file: \nP")
  (when rescale
    (unless (numberp rescale)
      (setq rescale (read-number "Rescale video to width (in pixels): "))
      (setq file (ewp-rescale-video file rescale))
      (push file ewp--deletable-files)))
  (insert (format "<video autoplay loop muted><source src=%S type=\"video/mp4\"></video>\n\n"
		  file)))

(defun ewp-insert-video-url (url)
  "Prompt for an URL and insert a <video> tag."
  (interactive "sVideo URL: ")
  (insert (format "<video autoplay loop muted><source src=%S type=\"video/mp4\"></video>\n\n"
		  url)))

(defun ewp-insert-tag (tag)
  "Insert a balanced pair of tags."
  (interactive (list (completing-read "Tag: " ewp-html-tags)))
  (insert "<" tag ">")
  (let ((point (point)))
    (insert "</" tag ">")
    (goto-char point)))

(defun ewp-tag-region (start end tag)
  "Insert a balanced pair of tags around the region."
  (interactive (list (mark) (point) (completing-read "Tag: " ewp-html-tags)))
  (insert "</" tag ">")
  (setq end (point))
  (goto-char start)
  (let ((string (concat "<" tag ">")))
    (insert string)
    (goto-char (+ end (length string)))))

(defun ewp-unfill-paragraph ()
  "Remove newlines from the current paragraph."
  (interactive)
  (save-excursion
    (let ((start (progn (forward-paragraph -1) (point)))
	  (end (progn (forward-paragraph 1) (point))))
      (goto-char start)
      (unless (bobp)
	(forward-line 1))
      (while (re-search-forward " *\n *" (1- end) t)
	(replace-match " " t t)))))

(defun ewp-download-and-insert-image ()
  "Download and insert the image from the URL in the kill ring."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (ewp-download-and-insert-image-1 url (current-buffer) (point))))

(defun ewp-download-and-insert-image-1 (url buffer point &optional callback)
  (url-retrieve
   url
   (lambda (_)
     (goto-char (point-min))
     (let (image)
       (when (search-forward "\n\n")
	 (setq image (buffer-substring (point) (point-max))))
       (kill-buffer (current-buffer))
       (when (and image
		  (buffer-live-p buffer))
	 (with-current-buffer buffer
	   (save-excursion
	     (goto-char (min point (point-max)))
	     (ewp-insert-image-data image)
	     (insert "\n\n"))
	   (when callback
	     (funcall callback))))))))

(defun ewp-insert-image-data (image)
  (insert-image
   (create-image image (ewp--image-type) t
		 :max-width (truncate (ewp--display-width))
		 :max-height (- (frame-pixel-height) 500))
   (format "<img src=\"data:%s;base64,%s\">"
	   (ewp-content-type image)
	   ;; Get a base64 version of the image.
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (insert image)
	     (base64-encode-region (point-min) (point-max) t)
	     (buffer-string)))
   nil nil t))

(defun ewp-content-type (image)
  ;; Get the MIME type by running "file" over it.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert image)
    (call-process-region (point-min) (point-max)
			 "file" t (current-buffer) nil
			 "--mime-type" "-")
    (cadr (split-string (buffer-string)))))

(defun ewp-yank-html ()
  "Yank the contents of the current X text/html selection, if any."
  (interactive)
  (let ((data (cl-loop for type in '(PRIMARY CLIPBOARD)
		       for data = (x-get-selection-internal type 'text/html)
		       when data
		       return data)))
    (if (not data)
	(message "No text/html data in the current selection")
      (set-mark (point))
      (insert (ewp-decode-text-selection data)))))

(defun ewp-decode-text-selection (data)
  (if (and (> (length data) 2)
	   (= (aref data 0) 255)
	   (= (aref data 1) 254))
      ;; Somehow the selection is UTF-16 when selecting text in
      ;; Firefox.
      (decode-coding-string data 'utf-16-le)
    ;; But some sources add a nul to the end of the data.
    (decode-coding-string
     (replace-regexp-in-string (string 0) "" data)
     'utf-8)))

(defun ewp-yank-picture ()
  "Yank the contents of the current X image selection/clipboard, if any."
  (interactive)
  (let ((data
	 (cl-loop for type in '(PRIMARY CLIPBOARD)
		  for st = (cl-loop for st across
				    (gui-get-selection type 'TARGETS)
				    when (equal (car (split-string
						      (symbol-name st) "/"))
						"image")
				    return st)
		  when st
		  return (x-get-selection-internal type st))))
    (if (not data)
	(message "No image data in the current selection/clipboard")
      (set-mark (point))
      (ewp-insert-image-data data)
      (insert "\n\n"))))

(defun ewp-remove-html-layer ()
  "Remove one layer of HTML tagging."
  (interactive)
  (save-excursion
    (unless (looking-at "<\\([^ ]+\\)[^>]+>")
      (search-backward "<" nil t))
    (when (looking-at "<\\([^ ]+\\)[^>]+>")
      (let ((tag (match-string 1)))
	(delete-region (match-beginning 0) (match-end 0))
	(when (re-search-forward (concat "</" (regexp-quote tag)
					 "\\(>\\| .*>\\)")
				 nil t)
	  (delete-region (match-beginning 0) (match-end 0)))))))

(defun ewp-clean-link (beg end)
  "Remove everything but the <a href=...>...</a> from the region."
  (interactive "r")
  (let ((a (car (dom-by-tag (libxml-parse-html-region beg end) 'a))))
    (if (not a)
	(error "No link in the region")
      (dom-set-attributes a (list (cons 'href (dom-attr a 'href))))
      (delete-region beg end)
      (ewp-print-html a))))

(defun ewp-print-html (dom &optional no-end-tag)
  "Convert DOM into a string containing the xml representation."
  (if (stringp dom)
      (insert dom)
    (insert (format "<%s" (car dom)))
    (dolist (attr (nth 1 dom))
      ;; Ignore attributes that start with a colon.
      (unless (= (aref (format "%s" (car attr)) 0) ?:)
        (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
    (insert ">")
    (dolist (elem (nthcdr 2 dom))
      (svg-print elem))
    (unless no-end-tag
      (insert (format "</%s>" (car dom))))))

(defun ewp-get-media-library (blog-xmlrpc user-name password blog-id count
					  &optional offset)
  "Retrieves list of images etc from the weblog system. Uses wp.getMediaLibrary."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getMediaLibrary"
                       blog-id
                       user-name
                       password
		       `(("number" . ,count)
			 ("offset" . ,(or offset 0)))))

(defconst ewp--thumbnail-placeholder
  (propertize
   " "
   'display (svg-image
	     (let ((svg (svg-create 150 150)))
	       (svg-gradient svg "gradient" 'linear
			     '((0 . "#202020") (100 . "#303030")))
	       (svg-rectangle svg 0 0 150 150 :gradient "gradient")
	       svg)
	     :scale 1)))

(defvar-keymap ewp-list-media-mode-map
  "g" #'ewp-list-media
  "w" #'ewp-copy-media
  "u" #'ewp-copy-url
  "m" #'ewp-upload-media
  "r" #'ewp-rotate-media
  "RET" #'ewp-show-media
  "DEL" #'ewp-delete-media
  "n" #'ewp-show-media-goto-next
  "SPC" #'ewp-toggle-media-mark
  "U" #'ewp-remove-media-marks
  ">" #'ewp-load-more-media)

(define-derived-mode ewp-list-media-mode special-mode "ewp"
  "Major mode for listing Wordpress media.

All normal editing commands are switched off.
\\<ewp-list-media-mode-map>"
  (setq truncate-lines t)
  (setq-local ewp-marks nil))

(defun ewp-list-media (&optional address old-media)
  "List the media on the ADDRESS blog."  
  (interactive)
  (let* ((address (or address ewp-address))
	 (media (nconc old-media
		       (ewp-call 'ewp-get-media-library address 500
				 (length old-media))))
	 marks)
    (switch-to-buffer (format "*%s media*" address))
    (setq marks (and (boundp 'ewp-marks) ewp-marks))
    (ewp-save-excursion
      (let ((inhibit-read-only t))
	(erase-buffer)
	(ewp-list-media-mode)
	(setq ewp-marks marks)
	(setq-local ewp-address address)
	(make-vtable
	 :columns '((:name "" :width 1)
		    (:name "Date" :width 10)
		    (:name "Thumbnail" :width "150px")
		    (:name "Name"))
	 :objects media
	 :getter
	 (lambda (post column vtable)
	   (pcase (vtable-column vtable column)
	     ("" (if (ewp-find-mark
		      (cdr (assoc "attachment_id" post)))
		     "*"
		   " "))
	     ("Date"
	      (format-time-string
	       "%Y-%m-%d"
	       (caddr (assoc "date_created_gmt" post))))
	     ("Thumbnail"
	      (if (string-match "^image/" (or (cdr (assoc "type" post)) ""))
		  (progn
		    (let* ((url (cdr (assoc "thumbnail" post)))
			   (cache (ewp--cache-file url)))
		      (if (file-exists-p cache)
			  (propertize
			   " "
			   'display (create-image cache nil nil :scale 1)
			   'local-map image-map)
			(propertize
			 ewp--thumbnail-placeholder
			 'thumbnail url))))
		(propertize
		 (let ((type (cdr (assoc "type" post))))
		   (cond
		    ((null type)
		     "")
		    ((string-match "^video/\\(.*\\)" type)
		     (match-string 1 type))
		    (t type)))
		 'face '(:foreground "#808080"))))
	     ("Name"
	      (cdr (assoc "title" post)))))
	 :keymap ewp-list-media-mode-map)))
    (ewp--insert-media-thumbnails)))

(defun ewp--insert-media-thumbnails ()
  (let ((buffer (current-buffer))
	func)
    (setq func
	  (lambda ()
	    (when (buffer-live-p buffer)
	      (with-current-buffer buffer
		(save-excursion
		  (goto-char (point-min))
		  (when-let ((match (text-property-search-forward 'thumbnail))
			     (url (prop-match-value match))
			     (cache (ewp--cache-file url)))
		    (if (file-exists-p cache)
			(progn
			  (ewp--insert-thumbnail
			   match buffer (create-image cache nil t :scale 1))
			  (run-at-time 0.1 nil func))
		      (ewp-url-retrieve
		       url
		       (lambda (&rest _)
			 (goto-char (point-min))
			 ;; These resources are pretty constant, so
			 ;; avoid re-fetching them.
			 (insert "Expires: Thu, 18 Apr 2224 10:52:21 +0200\n")
			 (url-store-in-cache)
			 (let (img)
			   (when (search-forward "\n\n" nil t)
			     (setq img (create-image
					(buffer-substring (point) (point-max))
					nil t :scale 1))
			     (let ((dir (file-name-directory cache)))
			       (unless (file-exists-p dir)
				 (make-directory dir t)))
			     (write-region (point) (point-max)
					   cache nil 'silent))
			   (kill-buffer (current-buffer))
			   (when (buffer-live-p buffer)
			     (ewp--insert-thumbnail match buffer img)
			     (run-at-time 0.1 nil func))))))))))))
    (run-at-time 0.1 nil func)))

(defun ewp--cache-file (url)
  (expand-file-name (concat (substring (sha1 url) 0 2)
			    "/"
			    (substring (sha1 url) 2))
		    "~/.cache/ewp/media/"))

(defun ewp--insert-thumbnail (match buffer img)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (prop-match-beginning match))
      (let ((inhibit-read-only t))
	(put-text-property (point)
			   (prop-match-end match)
			   'thumbnail nil)
	(when img
	  (add-text-properties
	   (point) (prop-match-end match)
	   (list 'display img
		 'local-map image-map)))))))

(defun ewp-load-more-media ()
  "Load more media from the blog."
  (interactive)
  (ewp-list-media ewp-address (ewp-current-data)))

(defun ewp-upload-media (file address)
  "Upload media files to Wordpress."
  (interactive
   (list (read-file-name "File to upload: ")
	 (or (and (boundp 'ewp-address) ewp-address)
	     (completing-read "Blog address: " ewp-blog-addresses))))
  (let* ((result (ewp-upload-file address file))
	 (url (cdr (assoc "url" result))))
    (setq url (replace-regexp-in-string "-scaled" "" url))
    (message "Uploaded %s to %s (copied to clipboard)"
	     file address)
    (with-temp-buffer
      (insert url)
      (copy-region-as-kill (point-min) (point-max)))))

(defun ewp-show-media-goto-next ()
  "Show the media under point.
If media is currently shown, advance to the next line and show
the media there instead."
  (interactive)
  (when (eq last-command 'ewp-show-media-goto-next)
    (forward-line 1))
  (ewp-show-media))

(defun ewp-show-media ()
  "Show the media under point."
  (interactive)
  (let* ((data (get-text-property (point) 'vtable-object))
	 (url (cdr (assoc "link" data))))
    (if (not data)
	(error "No media under point")
      (url-retrieve
       url
       (lambda (_)
	 (goto-char (point-min))
	 (let (image)
	   (when (search-forward "\n\n")
	     (setq image (buffer-substring (point) (point-max))))
	   (kill-buffer (current-buffer))
	   (when image
	     (with-temp-buffer
	       (insert-image (create-image image (ewp--image-type) t
					   :max-width 800))
	       (let ((max-mini-window-height 0.9))
		 (message "%s" (buffer-string)))))))))))

(defun ewp-delete-media-1 (url user password blog-id id)
  (xml-rpc-method-call url "wp.deletePost" blog-id user password
		       id))

(defun ewp-delete-media ()
  "Delete the media under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (if (not data)
	(error "No media under point")
      (let ((id (cdr (assoc "attachment_id" data))))
	(when (yes-or-no-p (format "Are you sure you want to delete media %s?"
				   id))
	  (let ((result (ewp-call 'ewp-delete-media-1 ewp-address id)))
	    (if (not (eq result t))
		(message "Got an error: %s" result)
	      (message "Media deleted")
	      (vtable-remove-object (vtable-current-table) data))))))))

(defun ewp-copy-urls-as-curl ()
  "Copy URLs as a series of curl commands."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (if (not (or ewp-marks data))
	(error "No media under point")
      (kill-new
       (mapconcat (lambda (elem)
		    (format "curl -O %S"
			    (replace-regexp-in-string
			     "-scaled" ""
			     (cdr (assoc "link" elem)))))
		  (or ewp-marks (list data))
		  "; "))
      (message "Copied"))))

(defun ewp-copy-media ()
  "Copy the media under point to the kill ring."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (if (not (or ewp-marks data))
	(error "No media under point")
      (ewp-copy-media-1 (or ewp-marks (list data))
			(length (or ewp-marks (list data)))))))

(defun ewp-copy-media-1 (elems length &optional prev)
  (setq elems (reverse elems))
  (let* ((elem (pop elems))
	 (url (cdr (assoc "link" elem)))
	 (id (cdr (assoc "attachment_id" elem))))
    (url-retrieve
     url
     (lambda (_)
       (goto-char (point-min))
       (let (image)
	 (when (search-forward "\n\n")
	   (setq image (buffer-substring (point) (point-max))))
	 (kill-buffer (current-buffer))
	 (when image
	   (with-temp-buffer
	     (when prev
	       (insert prev))
	     (insert-image
	      (create-image
	       image (ewp--image-type) t
	       :max-width (ewp--display-width)
	       :max-height
	       (truncate (* (frame-pixel-height) 0.8)))
	      (format "<a href=%S><img src=%S class=\"wp-image-%s\"></a>\n"
		      (replace-regexp-in-string "-scaled" "" url)
		      url id))
	     (insert "\n\n")
	     (if elems
		 (ewp-copy-media-1 elems length (buffer-string))
	       (copy-region-as-kill (point-min) (point-max))
	       (message "Copied %s image%s to the kill ring"
			length (if (= length 1) "" "s"))))))))))

(defun ewp-copy-url (&optional shortlink)
  "Copy the URL under point to the kill ring.
If SHORTLINK (interactively, the prefix), get a shortlink instead."
  (interactive "P")
  (let* ((data (get-text-property (point) 'vtable-object))
	 (url (cdr (assoc "link" data))))
    ;; Link to the unscaled version of the image.
    (setq url (replace-regexp-in-string "-scaled\\([.][^.]+\\'\\)" "\\1"
					url))
    (if (not data)
	(error "No media under point")
      (when shortlink
	(setq url (format "https://%s/?p=%s"
			  (url-host (url-generic-parse-url url))
			  (cdr (assoc "attachment_id" data)))))
      (with-temp-buffer
	(insert url)
	(copy-region-as-kill (point-min) (point-max))
	(message "Copied %s to the kill ring" url)))))

(defun ewp-rotate-media ()
  "Rotate the media under point."
  (interactive)
  (let* ((data (get-text-property (point) 'vtable-object))
	 (image (get-text-property (point) 'display))
	 (url (cdr (assoc "link" data)))
	 (image-name (cdr (assoc "title" data)))
	 (address ewp-address))
    ;; Link to the unscaled version of the image.
    (setq url (replace-regexp-in-string "-scaled\\([.][^.]+\\'\\)" "\\1"
					url))
    (if (or (not data)
	    (not image)
	    (not (eq (car image) 'image)))
	(error "No media under point")
      (url-retrieve
       url
       (lambda (_)
	 (goto-char (point-min))
	 (let (idata)
	   (when (search-forward "\n\n")
	     (setq idata (buffer-substring (point) (point-max))))
	   (kill-buffer (current-buffer))
	   (when idata
	     (let* ((result
		     (ewp--upload-file
		      address
		      image-name
		      (mailcap-file-name-to-mime-type image-name)
		      (with-temp-buffer
			(set-buffer-multibyte nil)
			(insert idata)
			(ewp-possibly-rotate-buffer image)
			(base64-encode-region (point-min) (point-max))
			(buffer-string))))
		    (url (replace-regexp-in-string "-scaled" ""
						   (cdr (assoc "url" result)))))
	       (kill-new url)
	       (message "Copied %s" url)))))))))

(defun ewp-import-screenshot (delay)
  "Take a screenshot and insert in the current buffer.
DELAY (the numeric prefix) says how many seconds to wait before
starting the screenshotting process."
  (interactive "p")
  (let ((image (ewp--take-screenshot delay)))
    (set-mark (point))
    (ewp-insert-image-data image)
    (insert "\n\n")
    (message "")))

(defun ewp-screenshot-imagemagick ()
  (call-process "import" nil (current-buffer) nil "jpeg:-"))

(defun ewp-screenshot-gnome ()
  (let ((file (ewp--temp-name "ewp-screenshot" ".jpg")))
    (unwind-protect
	(call-process "gnome-screenshot" nil nil nil
		      "-a" "-f" file)
      (when (file-exists-p file)
	(insert-file-contents-literally file)
	(delete-file file)))))

(defun ewp--take-screenshot (delay)
  (unless (executable-find "import")
    (error "Can't find ImageMagick import command on this system"))
  (cl-decf delay)
  (unless (zerop delay)
    (dotimes (i delay)
      (message "Sleeping %d second%s..."
	       (- delay i)
	       (if (= (- delay i) 1)
		   ""
		 "s"))
      (sleep-for 1)))
  (message "Take screenshot")
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (funcall ewp-screenshot-function)
    (buffer-string)))

(defun ewp-upload-screenshot (delay &optional address shortlink)
  "Take a screenshot and upload to the current Wordpress address.
DELAY (the numeric prefix) says how many seconds to wait before
starting the screenshotting process.

If SHORTLINK, return a \"/?p=42434\" link instead of the full URL."
  (interactive "p")
  (let* ((image (ewp--take-screenshot delay))
	 (result (ewp--upload-file
		  (or address ewp-address)
		  (format-time-string "%Y-%m-%d.jpg")
		  "image/jpeg"
		  (with-temp-buffer
		    (set-buffer-multibyte nil)
		    (insert image)
		    (base64-encode-region (point-min) (point-max))
		    (buffer-string))))
	 (url 
	  ;; Link to the unscaled version of the image.
	  (replace-regexp-in-string "-scaled\\([.][^.]+\\'\\)" "\\1"
				    (cdr (assoc "url" result)))))
    (when shortlink
      (setq url (format "https://%s/?p=%s"
			(url-host (url-generic-parse-url url))
			(cdr (assoc "attachment_id" result)))))
    (kill-new url)
    (message "Copied %s %S"
	     (propertize
	      " "
	      'display
	      (create-image image
			    'jpeg t
			    :max-width (/ (frame-pixel-width) 4)
			    :max-height (/ (frame-pixel-height) 4)))
	     url)))

;; Emacs can get slow when displaying very large images, so it's
;; convenient to be able to display a rescaled, smaller image.  But
;; when we're looking to interact with the image (to crop it, for
;; instance), we want to work on the original image.
(defun ewp--image-file (image)
  (or (plist-get (cdr image) :original-file)
      (plist-get (cdr image) :file)))

(defun ewp--identify (image)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if (plist-get (cdr image) :file)
	(insert-file-contents-literally (ewp--image-file image))
      (insert (plist-get (cdr image) :data)))
    (call-process-region (point-min) (point-max) "identify" t (current-buffer)
			 nil "-")
    (buffer-string)))

(defun ewp-image-size (image)
  (let ((size (mapcar #'string-to-number
		      (split-string
		       (nth 2 (split-string (ewp--identify image)))
		       "x"))))
    (cons (car size) (cadr size))))

(defun ewp-schedule ()
  "Insert a Schedule header with the current time."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (forward-line -1)
    (insert (format-time-string "Schedule: %FT%T\n"))
    (goto-char (point-min))
    (when (re-search-forward "^Status: draft" nil t)
      (replace-match "Status: publish"))))

(defun ewp-copy-link ()
  "Copy the URL of the blog post under point to the kill ring."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No post under point"))
    (let* ((link (cdr (assoc "link" data)))
	   (date (caddr (assoc "post_date" data)))
	   (url
	    (if (not (string-match "/[?]p=" link))
		;; This is not a draft to be published in the future.
		(replace-regexp-in-string "^http:" "https:" link)
	      (let ((parse (url-generic-parse-url link)))
		(format "https://%s/%s/%s/" (url-host parse)
			(format-time-string "%Y/%m/%d" date)
			(cdr (assoc "post_name" data)))))))
      (kill-new url)
      (message "Copied %S" url))))

(defun ewp-html-quote-region (start end)
  "Quote some HTML entities in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region start end)
      (while (re-search-forward "[<>&]" nil t)
	(replace-match
	 (pcase (match-string 0)
	   ("<" "&lt;")
	   (">" "&gt;")
	   ("&" "&amp;")))))))

(defun ewp-get-comments (blog-xmlrpc user-name password blog-id comments
				     &optional offset)
  "Retrieves list of posts from the weblog system. Uses wp.getComments."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getComments"
                       blog-id
                       user-name
                       password
		       `(("number" . ,comments)
			 ("offset" . ,(or offset 0)))))

(defvar-keymap ewp-list-comments-mode-map
  "g" #'ewp-list-comments
  "RET" #'ewp-display-comment
  "a" #'ewp-approve-comment
  "h" #'ewp-hold-comment
  "d" #'ewp-trash-comment
  "u" #'ewp-undelete-comment
  "r" #'ewp-make-comment
  "e" #'ewp-make-comment-edit
  ">" #'ewp-load-more-comments)

(define-derived-mode ewp-list-comments-mode special-mode "ewp"
  "Major mode for listing Wordpress comments.

All normal editing commands are switched off.
\\<ewp-list-comments-mode-map>"
  (buffer-disable-undo)
  (setq-local ewp-deleted-comments nil)
  (setq truncate-lines t))

(defun ewp-list-comments (&optional address old-data)
  "List the recent comments for the blog."
  (interactive)
  (let ((address (or address ewp-address)))
    (switch-to-buffer (format "*%s comments*" address))
    (ewp-list-comments-mode)
    (setq-local ewp-address address)
    (ewp-save-excursion
      (let ((inhibit-read-only t))
	(erase-buffer)
	(make-vtable 
	 :columns '((:name "Date")
		    (:name "Status" :width 10)
		    (:name "Author" :width 10)
		    (:name "Title" :width 15)
		    (:name "Comment" :width 100))
	 :objects (nconc old-data
			 (ewp-call 'ewp-get-comments address 1000
				   (length old-data)))
	 :getter
	 (lambda (comment column vtable)
	   (pcase (vtable-column vtable column)
	     ("Date" 
	      (format-time-string "%Y-%m-%d"
				  (caddr (assoc "date_created_gmt" comment))))
	     ("Status"
	      (propertize (cdr (assoc "status" comment))
			  'face '(:foreground "#a0a0a0")))
	     ("Author"
	      (propertize
	       (mm-url-decode-entities-string
		(or (cdr (assoc "author" comment)) ""))
	       'face `( :foreground "#b0b0b0"
			,@(if (not (equal (cdr (assoc "type" comment))
					  "pingback"))
			      (list :background "#505050")))))
	     ("Title"
	      (mm-url-decode-entities-string
	       (cdr (assoc "post_title" comment))))
	     ("Comment"
	      (mm-url-decode-entities-string
	       (replace-regexp-in-string
		"[\n ]+" " " (cdr (assoc "content" comment)))))))
	 :keymap ewp-list-comments-mode-map)))))

(defun ewp-load-more-comments ()
  "Load more comments from the blog."
  (interactive)
  (ewp-list-comments nil (ewp-current-data)))

(defun ewp-current-data ()
  (let ((data nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when-let ((elem (get-text-property (point) 'vtable-object)))
	  (push elem data))
	(forward-line 1)))
    (nreverse data)))

(defun ewp-display-comment ()
  "Display the comment under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No comment under point"))
    (switch-to-buffer (format "*%s comment*" (cdr (assoc "comment_id" data))))
    (eww-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eww-display-html
       'utf-8 "data:" 
       (with-temp-buffer
	 (insert
	  (if (cdr (assoc "author_url" data))
	      (format "<a href=%S>%s</a>"
		      (cdr (assoc "author_url" data))
		      (or (cdr (assoc "author" data)) "Somebody"))
	    (format "%s" (or (cdr (assoc "author" data)) "Somebody"))))
	 (insert (format " writes as a comment to <a href=%S>%s</a>:<p>"
			 (cdr (assoc "link" data))
			 (cdr (assoc "post_title" data))))
	 (insert (cdr (assoc "content" data)))
	 (libxml-parse-html-region (point-min) (point-max)))
       (point-min) (current-buffer))
      (goto-char (point-min)))))

(defvar ewp-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map))

(define-derived-mode ewp-comment-mode special-mode "ewp"
  "Major mode for displaying a Wordpress comment.

All normal editing commands are switched off.
\\<ewp-comment-mode-map>"
  (setq truncate-lines t))

(defun ewp-edit-comment (blog-xmlrpc user-name password blog-id comment-id data)
  "Edits an exiting comment."
  (xml-rpc-xml-to-response
   (xml-rpc-request
    blog-xmlrpc
    `((methodCall
       nil
       (methodName nil "wp.editComment")
       (params
	nil
        (param nil (value nil (string nil ,blog-id)))
        (param nil (value nil (string nil ,user-name)))
        (param nil (value nil (string nil ,password)))
        (param nil (value nil (string nil ,comment-id)))
        (param
	 nil
	 (value
	  nil
          (struct
           nil
           (member nil
                   (name nil "status")
                   (value nil ,(cdr (assoc "status" data))))
           (member nil
                   (name nil "dateCreated")
                   (value nil ,(ewp-external-time
				(caddr (assoc "date_created_gmt" data)))))
           (member nil
                   (name nil "content")
                   (value nil ,(cdr (assoc "content" data))))
           (member nil
                   (name nil "author")
                   (value nil ,(cdr (assoc "author" data))))
           (member nil
                   (name nil "author_url")
                   (value nil ,(cdr (assoc "author_url" data))))
           (member nil
                   (name nil "author_email")
                   (value nil ,(cdr (assoc "author_email" data)))))))))))))

(defun ewp-approve-comment ()
  "Approve the comment under point."
  (interactive)
  (ewp-change-status "approve")
  (forward-line 1))

(defun ewp-hold-comment ()
  "Unapprove the comment under point."
  (interactive)
  (ewp-change-status "hold")
  (forward-line 1))

(defun ewp-change-status (status)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No comment under point"))
    (setcdr (assoc "status" data) status)
    (let ((result (ewp-call 'ewp-edit-comment ewp-address
			    (cdr (assoc "comment_id" data)) data))
	  (inhibit-read-only t))
      (if (not (eq result t))
	  (message "Got an error: %s" result)
	(message "Updated comment successfully")
	(setcdr (assoc "status" data) status)
	(vtable-update-object (vtable-current-table) data data)))))

(defun ewp-delete-comment (blog-xmlrpc user-name password blog-id comment-id)
  "Deletes a comment system. Uses wp.deleteComment."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.deleteComment"
                       blog-id
                       user-name
                       password
		       comment-id))

(defun ewp-trash-comment ()
  "Trash (i.e., delete) the comment under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No comment under point"))
    (let ((result (ewp-call 'ewp-delete-comment ewp-address
			    (cdr (assoc "comment_id" data)))))
      (if (not (eq result t))
	  (message "Got an error: %s" result)
	(message "Comment deleted")
	(let ((inhibit-read-only t))
	  (push (buffer-substring (line-beginning-position)
				  (save-excursion
				    (forward-line 1) (point)))
		ewp-deleted-comments)
	  (delete-region (line-beginning-position)
			 (save-excursion
			   (forward-line 1) (point))))))))

(defun ewp-undelete-comment ()
  "Ressurrect the previously deleted comment."
  (interactive)
  (unless ewp-deleted-comments
    (error "No deleted comments in the undo queue"))
  (let* ((line (pop ewp-deleted-comments))
	 (data (get-text-property 1 'vtable-object line))
	 (inhibit-read-only t)
	 (result
	  (ewp-call 'ewp-edit-comment ewp-address
		    (cdr (assoc "comment_id" data))
		    data)))
    (if (not (eq result t))
	(message "Error: %s" result)
      (beginning-of-line)
      (insert line))))
  
(defun ewp-make-comment (&optional editp)
  "Post a new comment or a reply to the comment under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object))
	(address ewp-address))
    (unless data
      (error "No comment under point"))
    (switch-to-buffer (format "*comment %s*" (cdr (assoc "post_id" data))))
    (erase-buffer)
    (ewp-edit-mode)
    (when editp
      (setq-local ewp-edit data)
      (insert (cdr (assoc "content" data)))
      (goto-char (point-min)))
    (setq-local ewp-comment data)
    (setq-local ewp-post data)
    (setq-local ewp-address address)))

(defun ewp-make-comment-edit ()
  "Edit a comment."
  (interactive)
  (ewp-make-comment t))

(defun ewp-send-comment ()
  (let* ((editp (and (boundp 'ewp-edit)
		     ewp-edit))
	 (result
	  (if editp
	      (progn
		(setcdr (assoc "content" ewp-edit) (buffer-string))
		(ewp-call 'ewp-edit-comment ewp-address
			  (cdr (assoc "comment_id" ewp-edit))
			  ewp-edit))
	    (ewp-call 'ewp-new-comment ewp-address
		      (cdr (assoc "post_id" ewp-post))
		      `(("content" . ,(buffer-string))
			("author" . ,user-full-name))
		      (cdr (assoc "comment_id" ewp-comment))))))
    (if (or (numberp result)
	    (eq result t))
	(progn
	  (message "Comment %s" (if editp "edited" "posted"))
	  (bury-buffer))
      (message "Error while posting: %s" result))))

(defun ewp-new-comment (blog-xmlrpc user-name password blog-id post-id
				    data &optional comment-parent)
  "Edits an existing comment."
  (xml-rpc-xml-to-response
   (xml-rpc-request
    blog-xmlrpc
    `((methodCall
       nil
       (methodName nil "wp.newComment")
       (params
	nil
        (param nil (value nil (string nil ,blog-id)))
        (param nil (value nil (string nil ,user-name)))
        (param nil (value nil (string nil ,password)))
        (param nil (value nil (string nil ,post-id)))
        (param
	 nil
	 (value
	  nil
          (struct
           nil
           (member nil
                   (name nil "comment_parent")
                   (value nil ,comment-parent))
           (member nil
                   (name nil "content")
                   (value nil ,(cdr (assoc "content" data))))
           (member nil
                   (name nil "author")
                   (value nil ,(cdr (assoc "author" data))))
           (member nil
                   (name nil "author_url")
                   (value nil ,(cdr (assoc "author_url" data))))
           (member nil
                   (name nil "author_email")
                   (value nil ,(cdr (assoc "author_email" data)))))))))))))

(defun ewp-call (func address &rest args)
  (let ((auth (ewp-auth address)))
    (apply func
	   (ewp-xmlrpc-url address)
	   (cl-getf auth :user) (funcall (cl-getf auth :secret))
	   (format "%s" ewp-blog-id)
	   args)))

(defun ewp-dired-copy-as-kill (files)
  "Copy the marked images to the kill ring."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (with-temp-buffer
    (dolist (file files)
      (ewp-insert-image-data (with-temp-buffer
			       (set-buffer-multibyte nil)
			       (insert-file-contents-literally file)
			       (buffer-string)))
      (insert "\n\n"))
    (copy-region-as-kill (point-min) (point-max))
    (message "Copied %s image%s to the kill ring"
	     (length files) (if (= (length files) 1)
				""
			      "s"))))

(defun ewp-dired-upload-media (files)
  "Upload media files to Wordpress."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (let ((address
	 (if (> (length ewp-blog-addresses) 1)
	     (completing-read "Upload to blog: " ewp-blog-addresses nil t)
	   (car ewp-blog-addresses)))
	(results nil))
    (dolist (file files)
      (push (ewp-upload-file address file) results))
    (message "Uploaded %s file%s to %s (copied to clipboard)"
	     (length files)
	     (if (= (length files) 1)
		 ""
	       "s")
	     address)
    (with-temp-buffer
      (dolist (result (reverse results))
	(insert (cdr (assoc "url" result)) "\n"))
      (delete-char -1)
      (copy-region-as-kill (point-min) (point-max)))))

(defun ewp-toggle-media-mark ()
  "Toggle the mark on the media under point."
  (interactive)
  (let* ((data (get-text-property (point) 'vtable-object))
	 (id (cdr (assoc "attachment_id" data))))
    (unless data
      (error "No media under point"))
    (if (ewp-find-mark id)
	(setq ewp-marks (delq (ewp-find-mark id) ewp-marks))
      (push data ewp-marks))
    (vtable-update-object (vtable-current-table)
			  data data)))

(defun ewp-remove-media-marks ()
  "Remove the mark from all media items."
  (interactive)
  (save-excursion
    (dolist (elem (copy-sequence ewp-marks))
      (vtable-goto-object elem)
      (ewp-toggle-media-mark))))

(defun ewp-find-mark (id)
  (cl-loop for elem in ewp-marks
	   when (equal (cdr (assoc "attachment_id" elem)) id)
	   return elem))

(defun ewp-float-image (&optional right)
  "Float the image under point to the left (and make it smaller visually).
If given a prefix, float to the right instead."
  (interactive "P")
  (beginning-of-line)
  (let ((image (get-text-property (point) 'display))
	(start (point)))
    (unless image
      (error "No image under point to float"))
    (insert (format "<p style=\"clear: both;\"><p class=\"%s-img\">"
		    (if right "right" "left")))
    (put-text-property start (point)
		       'display (if right "⇢" "⇠")))
  (message "Floating image to the %s" (if right "right" "left")))

(defun ewp-fix-aspect-ratio ()
  "Fix the aspect ratio of the image under point."
  (interactive)
  (let ((image (get-text-property (point) 'display))
	new-data)
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    (let* ((data (cl-getf (cdr image) :data))
	   (inhibit-read-only t))
      (when (null data)
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally (ewp--image-file image))
	  (setq data (buffer-string))))
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(insert data)
	(call-process-region (point-min) (point-max)
			     "convert" t (current-buffer) nil
			     "-resize" "650x503!"
			     (format "%s:-" (car (last (split-string
							(ewp-content-type data)
							"/"))))
			     "jpg:-")
	(setq new-data (buffer-string)))
      (delete-region (line-beginning-position) (line-end-position))
      (ewp-insert-image-data new-data))))

(defun ewp-save-image (filename)
  "Save the image under point."
  (interactive "FFilename: ")
  (let ((image (get-text-property (point) 'display)))
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    (let* ((data (cl-getf (cdr image) :data))
	   (inhibit-read-only t))
      (when (null data)
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally (ewp--image-file image))
	  (setq data (buffer-string))))
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(insert data)
	(write-region (point-min) (point-max) filename))
      (delete-region (line-beginning-position) (line-end-position 2)))))

(defun ewp-delete-post (blog-xmlrpc user-name password _blog-id post-id)
  "Delete an entry from the weblog system."
  (xml-rpc-method-call blog-xmlrpc
                       "blogger.deletePost"
                       nil post-id user-name password t))

(defun ewp-trash-post ()
  "Trash (i.e., delete) the post under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object))
	(inhibit-read-only t))
    (unless data
      (error "No post under point"))
    (when (yes-or-no-p "Really delete this post? ")
      (let* ((auth (ewp-auth ewp-address))
	     (all-data (xml-rpc-method-call
			(ewp-xmlrpc-url ewp-address)
			"metaWeblog.getPost"
			(cdr (assoc "post_id" data))
			(cl-getf auth :user) (funcall (cl-getf auth :secret)))))
	(put-text-property (line-beginning-position)
			   (line-end-position)
			   'all-data all-data)
	(let ((result (ewp-call 'ewp-delete-post ewp-address
				(cdr (assoc "post_id" data)))))
	  (if (not (eq result t))
	      (message "Got an error: %s" result)
	    (message "Post deleted")
	    (push (buffer-substring (line-beginning-position)
				    (save-excursion
				      (forward-line 1) (point)))
		  ewp-deleted-posts)
	    (delete-region (line-beginning-position)
			   (save-excursion
			     (forward-line 1) (point)))))))))

(defun ewp-undelete-post ()
  "Ressurrect the previously deleted post."
  (interactive)
  (unless ewp-deleted-posts
    (error "No deleted posts in the undo queue"))
  (let* ((line (pop ewp-deleted-posts))
	 (data (get-text-property 1 'all-data line))
	 (inhibit-read-only t)
	 (auth (ewp-auth ewp-address))
	 (result
	  (ewp-blog-post
	   nil (ewp-get "post_id" data)
	   (ewp-xmlrpc-url ewp-address)
	   (cl-getf auth :user)
	   (funcall (cl-getf auth :secret))
	   ewp-blog-id data nil)))
    (if (not (eq result t))
	(message "Error: %s" result)
      (beginning-of-line)
      (insert line))))

(defun ewp-set-image-width (width)
  "Set the width of the image under point."
  (interactive "nImage width: ")
  (let ((image (get-text-property (point) 'display)))
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    (setf (cl-getf (cdr image) :width) width)))

(defun ewp-get-post-data (category)
  (cl-loop for elem in (ewp-call 'ewp-get-posts ewp-address 300 0 nil
				 ["post_title" "post_date" "post_status"
				  "terms" "link" "post_name" "post_content"])
	   when (or (null category)
		    (member category (ewp--categories elem)))
	   collect elem))

(defun ewp-toggle-thumbnail ()
  "Toggle whether the image under point is the thumbnail for the post."
  (interactive)
  (let ((image (get-text-property (point) 'display))
	match status)
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    (if (setq status (get-text-property (point) 'ewp-thumbnail))
	(put-text-property (point) (1+ (point)) 'ewp-thumbnail nil)
      (save-excursion
	(while (setq match (text-property-search-forward 'ewp-thumbnail))
	  (put-text-property (prop-match-beginning match)
			     (prop-match-end match)
			     'ewp-thumbnail nil)))
      (put-text-property (point) (1+ (point)) 'ewp-thumbnail t))
    (message "Made image under point %s" (if status "not the thumbnail"
					   "the thumbnail"))))

(defun ewp-reupload-images ()
  (goto-char (point-min))
  (while (re-search-forward "<img [^>\n]+>" nil t)
    (let* ((address ewp-address)
	   (start (match-beginning 0))
	   (end (match-end 0))
	   (dom (nth 2 (nth 2 (libxml-parse-html-region start end))))
	   (url (dom-attr dom 'src))
	   (class (dom-attr dom 'class)))
      ;; Do just PNGs and JPGs -- animated GIFs and the like probably
      ;; don't need this treatment.
      (when (and url
		 (string-match (format "\\`https://%s/wp-content/uploads/\\([0-9][0-9][0-9][0-9]/[0-9][0-9]\\)/\\(.*?\\(jpe?g\\|png\\)\\)\\(\\'\\|[?]\\)"
				       address)
			       url))
	(let ((date (match-string 1 url))
	      (file (match-string 2 url))
	      result)
	  (message "Fetching %s..." url)
	  (with-current-buffer (url-retrieve-synchronously url t)
	    (goto-char (point-min))
	    (when (re-search-forward "\n\n" nil t)
	      (base64-encode-region (point) (point-max))
	      (message "Uploading %s..." url)
	      (setq result
		    (ewp--upload-file
		     address file
		     (mailcap-file-name-to-mime-type file)
		     (buffer-substring (point) (point-max))
		     date)))
	    (kill-buffer (current-buffer)))
	  (when result
	    (let ((new-id (format "wp-image-%s" (cdr (assoc "id" result)))))
	      (goto-char start)
	      (delete-region start end)
	      ;; In normal Wordpress, the URL will change here, but if
	      ;; you're using this, you should alter Wordpress to
	      ;; overwrite the media file here.
	      (dom-set-attribute dom 'src (cdr (assoc "url" result)))
	      (dom-set-attribute
	       dom 'class
	       (cond
		;; Change the old ID.
		((and class
		      (string-match "wp-image-\\([0-9]+\\)" class))
		 (replace-regexp-in-string "wp-image-\\([0-9]+\\)"
					   new-id class))
		(class
		 (concat class " " new-id))
		(t
		 new-id)))
	      ;; Insert the new data.
	      (dom-print dom))))))))

(defun ewp-reupload-article (times)
  (interactive "p")
  (dotimes (_ times)
    (let ((point (point))
	  (buffer (current-buffer)))
      (ewp-select-post)
      (ewp-reupload-images)
      (ewp-update-post)
      (set-buffer buffer)
      (goto-char point)
      (forward-line -1)
      (set-window-point (selected-window) (point)))))

(defun ewp-edit-url (url)
  "Parse URL and try to find the correct blog and post to edit."
  (interactive "sEdit URL: ")
  (let ((parsed (url-generic-parse-url url)))
    (unless (member (url-host parsed) ewp-blog-addresses)
      (error "Can't find %s in `ewp-blog-addresses'" (url-host parsed)))
    (with-current-buffer (url-retrieve-synchronously url)
      (unless (re-search-forward "<body.*postid-\\([0-9]+\\)" nil t)
	(kill-buffer (current-buffer))
	(error "Couldn't find the blog post id"))
      (let ((id (match-string 1)))
	(kill-buffer (current-buffer))
	(ewp-select-post (url-host parsed) id)))))

(defun ewp-composite-image (logo size-ratio placement &optional callback)
  "Composite LOGO (a file) on top of the image under point.
SIZE-RATIO should be a floating point number smaller than 1.

PLACEMENT should be a function that returns x/y coordinates.
It's called with four parameters: width/height of the image, and
width/height of the logo.

If given, CALLBACK should be a function that will be called with
the SVG object and width/height of the SVG, and should return an
SVG object (possibly the same)."
  (interactive "P")
  (let ((image (get-text-property (point) 'display))
	lpos)
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    (setq image (copy-sequence image))
    (setf (plist-get (cdr image) :max-height) nil)
    (setf (plist-get (cdr image) :max-width) nil)
    (setf (plist-get (cdr image) :scale) nil)
    (let* ((data (cl-getf (cdr image) :data))
	   (type (cond
		  ((cl-getf (cdr image) :format)
		   (format "%s" (cl-getf (cdr image) :format)))
		  (data
		   (ewp-content-type data))))
	   svg logo-type logo-size lwidth lheight size)
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(if (null data)
	    (insert-file-contents-literally (ewp--image-file image))
	  (insert data))
	(let ((ewp-exif-rotate nil))
	  (ewp-possibly-rotate-buffer image))
	(setq data (buffer-string))
	(setq type (ewp-content-type data)))
      (setq size (image-size (create-image data nil t :scale 1) t))
      (setq svg
	    (svg-create (car size) (cdr size)
			:xmlns:xlink "http://www.w3.org/1999/xlink"
			:stroke-width 5))
      (svg-embed svg data type t
		 :width (car size)
		 :height (cdr size))
      (svg-embed svg
		 (with-temp-buffer
		   (insert-file-contents-literally logo)
		   (setq logo-type (ewp-content-type data))
		   (setq logo-size (image-size
				    (create-image logo nil nil
						  :scale 1)
				    t))
		   (buffer-string))
		 logo-type t
		 :width (setq lwidth (* (car size) size-ratio))
		 :height (setq lheight
			       (* (/ lwidth (float (car logo-size)))
				  (cdr logo-size)))
		 :x (progn
		      (setq lpos (funcall placement
					  (car size) (cdr size)
					  lwidth lheight))
		      (car lpos))
		 :y (cdr lpos))
      (when callback
	(setq svg (funcall callback svg (car size) (cdr size))))
      (delete-region (line-beginning-position)
		     (line-end-position))
      (ewp-insert-image-data
       (with-temp-buffer
	 (set-buffer-multibyte nil)
	 (svg-print svg)
	 (write-region (point-min) (point-max) "/tmp/ewp.svg")
	 (erase-buffer)
	 (call-process "convert" nil nil nil
		       "/tmp/ewp.svg" "/tmp/ewp.jpg")
	 (insert-file-contents-literally "/tmp/ewp.jpg")
	 (delete-file "/tmp/ewp.jpg")
	 (delete-file "/tmp/ewp.svg")
	 (buffer-string))))))

(defun ewp-get (key alist)
  (cdr (assoc key alist)))

(defun ewp-trim-image (fuzz)
  "Trim (i.e., remove black borders) the image under point.
FUZZ (the numerical prefix) says how much fuzz to apply."
  (interactive "p")
  (let ((image (get-text-property (point) 'display))
	new-data)
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    (let* ((data (cl-getf (cdr image) :data))
	   (inhibit-read-only t))
      (when (null data)
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally (ewp--image-file image))
	  (setq data (buffer-string))))
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(insert data)
	(call-process-region (point-min) (point-max)
			     "convert" t (current-buffer) nil
			     "-trim" "+repage"
			     "-fuzz" (format "%d%%" fuzz)
			     (format "%s:-" (car (last (split-string
							(ewp-content-type data)
							"/"))))
			     "jpg:-")
	(setq new-data (buffer-string)))
      (delete-region (line-beginning-position) (line-end-position))
      (ewp-insert-image-data new-data))))

(defun ewp-copy-title-and-url ()
  "Copy the title and the URL of the post under point."
  (interactive)
  (let ((data (get-text-property (point) 'vtable-object)))
    (unless data
      (error "No post under point"))
    (let* ((link (cdr (assoc "link" data)))
	   (date (caddr (assoc "post_date" data)))
	   (url
	    (if (not (string-match "/[?]p=" link))
		;; This is not a draft to be published in the future.
		(replace-regexp-in-string "^http:" "https:" link)
	      (let ((parse (url-generic-parse-url link)))
		(format "https://%s/%s/%s/" (url-host parse)
			(format-time-string "%Y/%m/%d" date)
			(cdr (assoc "post_name" data))))))
	   (string (concat (cdr (assoc "post_title" data))
			   "\n\n"
			   url)))
      (kill-new string)
      (message "Copied %S" string))))

(defun ewp-image-cut (&optional color)
  "Wrapper for `image-cut'."
  (interactive (list (and current-prefix-arg (read-color "Use color: "))))
  (ewp-image-crop (if (zerop (length color)) image-cut-color color)))

(defun ewp-image-crop (&optional cut)
  "Wrapper for `image-crop'."
  (interactive)
  ;; Swap back the original image file if we're displaying a smaller
  ;; resized one.
  (when-let* ((image (get-text-property (point) 'display))
	      (file (and (consp image)
			 (eq (car image) 'image)
			 (plist-get (cdr image) :file)
			 (plist-get (cdr image) :original-file))))
    (plist-put (cdr image) :file file))
  (image-crop cut))

(defun ewp-image-view ()
  "View the image under point with an external viewer."
  (interactive)
  (let* ((image (get-text-property (point) 'display))
	 (file (and (consp image)
		    (eq (car image) 'image)
		    (ewp--image-file image))))
    (unless file
      (user-error "No image with a file under point"))
    (start-process "feh" nil "feh" "-ZF" (expand-file-name file))))

(defun ewp-rotate-all-images ()
  "Rotate all images 270 degrees."
  (interactive)
  (while-let ((match (text-property-search-forward
		      'display nil (lambda (_ prop)
				     (and (consp prop)
					  (eq (car prop) 'image))))))
    (setf (image-property (prop-match-value match) :rotation)
	  (float 270))))

(defvar ewp-watch-directory-rescale nil
  "Size to rescale images to when inserting.
This should be a string like \"2048x\".")
  
(defvar ewp-watch-directory-trim-fuzz "4%"
  "Fuzz to use when trimming images.")

(defun ewp-watch-directory (directory &optional match no-ignore-existing
				      separator trim)
  "Watch DIRECTORY for new files and insert them in the buffer when they appear.
If MATCH, insert the files that match this name.  Defaults to .JPG.

If NO-IGNORE-EXISTING, don't ignore existing files in DIRECTORY.

If SEPARATOR, it should be a string to insert after inserting an image.

If TRIM, automatically crop images.  This is useful for
screenshots from TV, for instance."
  (interactive "DDirectory to watch: ")
  (let* ((data (make-vector 1 nil))
	 (files (if no-ignore-existing
		    nil
		  (directory-files directory t)))
	 (buffer (current-buffer))
	 desc)
    (setf (elt data 0) files)
    (setq desc (file-notify-add-watch
		"/home/sony/ftp/" '(change)
		(lambda (event)
		  (cond
		   ((eq (cadr event) 'stopped)
		    nil)
		   ((not (buffer-live-p buffer))
		    ;; Cancel ourself if the buffer is killed.
		    (file-notify-rm-watch (car event)))
		   ((memq (cadr event) '(created changed))
		    (ewp--watch-directory
		     (nth 2 event)
		     data files directory buffer match
		     separator trim))))))
    (push desc ewp--notification-descriptors)
    desc))

(defun ewp--watch-directory (file data orig-files directory buffer match
				  separator trim)
  (let ((files (elt data 0))
	new)
    (when (and (not (member file files))
	       (string-match (or match "[.][Jj][Pp][Gg]\\'")
			     (file-name-nondirectory file))
	       (plusp (file-attribute-size (file-attributes file)))
	       (ewp--file-complete-p file))
      (when (or ewp-watch-directory-rescale trim)
	(let ((crop (and trim
			 (ewp--find-crop
			  buffer directory (or match "[.][Jj][Pp][Gg]\\'")
			  orig-files))))
	  (setq new (ewp--uniqify-file-name
		     (expand-file-name
		      (file-name-nondirectory file) "/tmp/ewp/")))
	  (apply
	   #'call-process
	   `("convert" nil nil nil
	     ,@(if ewp-watch-directory-rescale
		   `("-scale" ,ewp-watch-directory-rescale))
	     ,@(if trim
		   (if crop
		       ;; If we have a crop factor, use it.
		       (list "-crop"
			     (apply #'format "%dx%d+%d+%d" crop))
		     ;; If not, auto-fuzz.
		     (list "-trim" "-fuzz"
			   ewp-watch-directory-trim-fuzz)))
	     ,file ,new)))
	(push file files)
	(setq file new)
	(push new ewp--deletable-files))
      (with-current-buffer buffer
	(let ((edges (window-inside-pixel-edges
		      (get-buffer-window (current-buffer) t))))
	  (save-excursion
	    (goto-char (point-max))
	    ;; If we're just after an image, leave
	    ;; several empty lines (to type in).  If not,
	    ;; just one empty line.
	    (if (save-excursion
		  (and (re-search-backward "^[^\n]" nil t)
		       (looking-at "<img")))
		(ensure-empty-lines 3)
	      (ensure-empty-lines 1))
	    (let ((start (point))
		  ;; Emacs can get really slow when
		  ;; displaying large images.  So resize
		  ;; and display a smaller one instead.
		  (smaller-file
		   (concat
		    (ewp--temp-name "wd" (file-name-extension file))
		    (file-name-nondirectory file))))
	      (push smaller-file ewp--deletable-files)
	      (call-process "convert" nil nil nil
			    "-resize" "800x"
			    file smaller-file)
	      (insert-image
	       (create-image
		smaller-file (ewp--image-type) nil
		:max-width
		(truncate
		 (* 0.95 (- (nth 2 edges) (nth 0 edges))))
		:max-height
		(truncate
		 (* 0.7 (- (nth 3 edges) (nth 1 edges))))
		:rotation
		(exif-orientation
		 (ignore-error exif-error
		   (exif-parse-file file))))
	       (format "<img src=%S>" file))
	      (put-text-property start (point) 'help-echo file)
	      (plist-put (cdr (get-text-property start 'display))
			 :original-file file))
	    (insert "\n\n")
	    (when separator
	      (insert separator)))))
      ;; Keep track of the inserted files.
      (push file files)
      (setf (elt data 0) files))))

(defun ewp--file-complete-p (file)
  "Say whether FILE has been completely written to file."
  (with-temp-buffer
    (zerop (call-process "identify" nil t nil
			 "-regard-warnings" file))))

(defvar-local ewp--crop-factor nil)

(defun ewp--find-crop (buffer dir match &optional ignore-files)
  "Find a crop factor based on the files in DIR matching MATCH."
  (with-current-buffer buffer
    (or ewp--crop-factor
	(when-let* ((files (seq-take (reverse
				      (seq-difference
				       (directory-files dir t match)
				       ignore-files))
				     10))
		    (crop (meme--find-crop files)))
	  ;; If we have a reasonable number of files, then cache the
	  ;; results so that things are less slow.
	  (when (> (length files) 9)
	    (setq-local ewp--crop-factor crop))
	  crop))))

(defun ewp--uniqify-file-name (file)
  (let ((num 2))
    (while (file-exists-p file)
      (setq file (replace-regexp-in-string "\\(-[0-9]+\\)?[.]"
					   (format "-%d." num) file))
      (cl-incf num)))
  file)

(defun ewp--temp-name (prefix &rest bits)
  (let* ((dir "/tmp/ewp/")
	 (file (apply #'concat
		      (make-temp-name (expand-file-name prefix dir))
		      bits)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (expand-file-name file dir)))

(defun ewp-rescale-video (file width)
  "Rescale FILE and convert into mp4."
  (interactive "fVideo file: \nnWidth (in pixels): ")
  (let ((output (ewp--temp-name "video-" ".mp4")))
    (call-process "ffmpeg" nil nil nil
		  "-i" file
		  "-vf" (format "scale=%d:-1" width) output)
    output))

(provide 'ewp)

;;; ewp.el ends here
