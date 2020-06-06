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

;; xml-rpc from https://github.com/hexmode/xml-rpc-el
;; metaweblog from https://github.com/org2blog/metaweblog
;; vpt from https://github.com/larsmagne/vpt.el

;; # apt install exiftool
;; if you want images to be properly rotated.

;; `M-x ewp' to get started.

;; If you have several blogs you can list them all:

;; (setq ewp-blog-addresses '("my.example.com" "other.foo.bar"))


;;; Code:

(require 'cl)
(require 'metaweblog)
(require 'mm-url)
(require 'dired)
(require 'vpt)
(require 'eww)
(require 'sgml-mode)

(defvar ewp-blog-address nil
  "The name/address of the blog, like my.example.blog.")

(defvar ewp-blog-addresses nil
  "A list of name/address of several blogs.")

(defvar ewp-blog-id 1
  "The Wordpress ID of the blog, which is usually 1.")

(defvar ewp-send-hook nil
  "Hook functions run after posting/editing a blog article.")

(defvar ewp-image-width 840
  "What width to tell Wordpress to resize images to when displaying on the blog.")

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

(defvar ewp-post)
(defvar ewp-address)
(defvar ewp-categories)
(defvar ewp-comment)
(defvar ewp-edit)
(defvar ewp-marks)
(defvar ewp-deleted-comments)
(defvar ewp-deleted-posts)

(defvar ewp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "e" 'ewp-select-post)
    (define-key map "p" 'ewp-preview)
    (define-key map "M" 'ewp-list-media)
    (define-key map "n" 'ewp-new-post)
    (define-key map "N" 'ewp-new-page)
    (define-key map "g" 'ewp-blog)
    (define-key map "s" 'ewp-list-posts-with-status)
    (define-key map "\r" 'ewp-browse)
    (define-key map "w" 'ewp-copy-link)
    (define-key map "d" 'ewp-trash-post)
    ;;(define-key map "u" 'ewp-undelete-post)
    (define-key map "c" 'ewp-make-comment)
    (define-key map "C" 'ewp-list-comments)
    (define-key map "A" 'ewp-list-posts-with-category)
    (define-key map ">" 'ewp-load-more-posts)
    map))

(define-derived-mode ewp-list-mode special-mode "ewp"
  "Major mode for listing Wordpress posts.

All normal editing commands are switched off.
\\<ewp-list-mode-map>"
  (setq truncate-lines t)
  (setq-local ewp-deleted-posts nil))

(defun ewp--display-width ()
  (or ewp-display-width
      (- (frame-pixel-width) 50)))

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
	:data (get-text-property (point) 'data)
	:size (buffer-size)
	:line (count-lines (point-min) (point))))

(defun ewp-restore-location (loc)
  (cond
   ;; We started with an empty buffer, so place point after the header
   ;; line.
   ((zerop (getf loc :size))
    (goto-char (point-min))
    (unless (eobp)
      (forward-line 1)))
   ;; Find a specific element we were on.
   ((getf loc :data)
    (goto-char (point-min))
    (while (and (not (eobp))
		(not (ewp-item-equal
		      (getf loc :data) (get-text-property (point) 'data))))
      (forward-line 1)))
   ;; Go to the same numeric line.
   (t
    (goto-char (point-min))
    (forward-line (getf loc :line)))))

(defun ewp-item-equal (e1 e2)
  (loop for name in '("page_id" "post_id" "comment_id" "attachment_id")
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

(defun ewp-blog (&optional address old-data status category all)
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
	  lines data)
      (erase-buffer)
      (ewp-list-mode)
      (setq-local ewp-address address)
      (dolist (post (nconc old-data
			   (ewp-call 'ewp-get-posts address
				     (if all 30000 300)
				     (length old-data) status)))
	(when (or (null category)
		  (member category (ewp--categories post)))
	  (push post data)
	  (push (ewp-make-entry post) lines)))
      (when (and (not old-data)
		 (not status))
	(dolist (post (ewp-call 'wp-get-pagelist address))
	  (push post data)
	  (push (ewp-make-entry post) lines)))
      (variable-pitch-table '((:name "Date" :width 10)
			      (:name "Status" :width 10)
			      (:name "Categories" :width 15)
			      (:name "Title"))
			    (nreverse lines)
			    (nreverse data)))))

(defun ewp-load-more-posts (&optional all)
  "Load more posts from the blog.
If ALL (the prefix), load all the posts in the blog."
  (interactive "P")
  (ewp-blog ewp-address (ewp-current-data) nil nil t))

(defun ewp-make-entry (post)
  (let* ((prefix (if (assoc "page_title" post)
		     "page"
		   "post"))
	 (date (or (caddr (assoc "post_date" post))
		   (caddr (assoc "date_created_gmt" post))))
	 (status (cdr (assoc (format "%s_status" prefix) post))))
    (when (and (equal status "publish")
	       (time-less-p (current-time) date))
      (setq status "schedule"))
    (list
     (propertize
      (format-time-string "%Y-%m-%d" date)
      'face 'variable-pitch)
     (propertize 
      (or status "")
      'face '(variable-pitch :foreground "#a0a0a0"))
     (propertize
      (mapconcat 'identity (ewp--categories post) ",")
      'face '(variable-pitch :foreground "#b0b0b0"))
     (propertize
      (mm-url-decode-entities-string
       (or (cdr (assoc (format "%s_title" prefix) post)) ""))
      'face 'variable-pitch))))

(defun ewp--categories (post)
  (loop for term in (cdr (assoc "terms" post))
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
				  fields)
  "Retrieves list of posts from the weblog system. Uses wp.getPosts."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPosts"
                       blog-id
                       user-name
                       password
		       `(("number" . ,posts)
			 ("offset" . ,(or offset 0))
			 ,@(and status (list `("post_status" . ,status))))
		       (or fields
			   ["post_title" "post_date" "post_status" "terms"
			    "link" "post_name"])))

(defun ewp-get-page (blog-xmlrpc user-name password page-id)
  "Retrieves a page from the weblog. PAGE-ID is the id of the post
which is to be returned.  Can be used with pages as well."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPage"
		       nil
                       page-id
                       user-name
                       password))

(defun ewp-xmlrpc-url (address)
  (format "https://%s/xmlrpc.php"
	  (or (cadr (assoc address ewp-address-map))
	      address)))

(defun ewp-select-post ()
  "Edit the post under point."
  (interactive)
  (let* ((data (get-text-property (point) 'data))
	 (pagep (assoc "page_id" data))
	 (id (if pagep
		 (cdr (assoc "page_id" data))
	       (cdr (assoc "post_id" data))))
	 (auth (ewp-auth ewp-address))
	 (post (funcall
		(if pagep
		    'ewp-get-page
		  'metaweblog-get-post)
		(ewp-xmlrpc-url ewp-address)
		(getf auth :user) (funcall (getf auth :secret))
		id))
	 (date (or (caddr (assoc "post_date" post))
		   (caddr (assoc "date_created_gmt" post))))
	 (status (cdr (assoc (if pagep
				 "page_status"
			       "post_status")
			     post)))
	 (address ewp-address))
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
      (insert (format-time-string "Schedule: %FT%T\n" date)))
    (insert "\n")
    (insert (or (cdr (assoc "description" post)) ""))
    (goto-char (point-min))
    (ewp-save-buffer id)
    (setq-local ewp-post post)))

(defun ewp-update-images ()
  (ewp-update-image
   (loop for img in (dom-by-tag (libxml-parse-html-region (point) (point-max))
				'img)
	 repeat 100
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
				(create-image image (ewp--image-type) t
					      :max-width (ewp--display-width)
					      :max-height (- (frame-pixel-height) 50)
					      :format content-type)
				'keymap image-map)))))))))
	   (kill-buffer buf))
	 (when (buffer-live-p buffer)
	   (ewp-update-image urls buffer)))))))

(defun ewp-sort-date (e1 e2)
  (time-less-p (caddr (assoc "post_date" e1))
	       (caddr (assoc "post_date" e2))))

(defvar ewp-edit-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-a" 'ewp-yank-with-href)
    (define-key map "\C-c\C-y" 'ewp-yank-link-with-text)
    (define-key map "\C-c\C-b" 'ewp-yank-with-blockquote)
    (define-key map "\C-c\C-c" 'ewp-update-post)
    (define-key map "\C-c\C-d" 'ewp-download-and-insert-image)
    (define-key map "\C-c\C-i" 'ewp-insert-img)
    (define-key map "\C-c\C-v" 'ewp-insert-video)
    (define-key map "\C-c\C-l" 'ewp-remove-html-layer)
    (define-key map "\C-c\C-m" 'ewp-yank-html)
    (define-key map "\C-c\C-n" 'ewp-clean-link)
    (define-key map "\C-c\C-o" 'ewp-html-quote-region)
    (define-key map "\C-c\C-p" 'ewp-yank-picture)
    (define-key map "\C-c\C-q" 'ewp-remove-image-thumbnails)
    (define-key map "\C-c\C-w" 'ewp-insert-image-thumbnails)
    (define-key map "\C-c\C-r" 'ewp-tag-region)
    (define-key map "\C-c\C-s" 'ewp-import-screenshot)
    (define-key map "\C-c\C-t" 'ewp-insert-tag)
    (define-key map "\C-c\C-u" 'ewp-unfill-paragraph)
    (define-key map "\C-c\C-z" 'ewp-schedule)
    (define-key map "\C-c\C-k" 'ewp-crop-image)
    (define-key map (kbd "C-c C-S-t") 'ewp-trim-image)
    (define-key map "\C-c\C-j" 'ewp-set-image-width)
    (define-key map "\t" 'ewp-complete)
    (define-key map (kbd "C-c C-$") 'ewp-toggle-thumbnail)
    map))

(define-derived-mode ewp-edit-mode text-mode "ewp"
  "Major mode for editing Wordpress posts.
\\<ewp-edit-mode-map>"
  (setq-local word-wrap t)
  (setq-local normal-auto-fill-function 'ignore)
  (setq-local completion-at-point-functions
	      (cons
	       'ewp-complete-status
	       (cons 'ewp-complete-category completion-at-point-functions)))
  (auto-save-mode 1))

(defun ewp-update-post ()
  "Update the post in the current buffer on Wordpress."
  (interactive)
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
	    (post (copy-list (append ewp-post
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
	(apply
	 (if pagep
	     (if (cdr (assoc "page_id" post))
		 'wp-edit-page
	       'wp-new-page)
	   (if ewp-post
	       'metaweblog-edit-post
	     'metaweblog-new-post))
	 `(,(ewp-xmlrpc-url ewp-address)
	   ,(getf auth :user)
	   ,(funcall (getf auth :secret))
	   ,@(if pagep
		 (list (format "%s" ewp-blog-id))
	       nil)
	   ,@(if (or (not pagep)
		     (cdr (assoc "page_id" post)))
		 (list (format "%s" (if pagep
					(cdr (assoc "page_id" post))
				      (cdr (assoc "postid" post)))))
	       nil)
	   ,post
	   ;; Publish if already published.
	   ,(equal (cdr (assoc "Status" headers)) "publish")))
	(set-buffer-modified-p nil)
	(message "%s the post"
		 (if ewp-post
		     "Edited"
		   "Posted"))
	(bury-buffer)))))

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

(defun ewp-new-post (&optional address buffer)
  "Start editing a new post."
  (interactive)
  (let ((address (or address ewp-address)))
    (switch-to-buffer (or buffer (generate-new-buffer "*Wordpress Post*")))
    (ewp-edit-mode)
    (setq-local ewp-post nil)
    (setq-local ewp-address address)
    (insert "Title: \nCategories: \nStatus: draft\n\n")
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

(defun ewp-upload-file (address file &optional image)
  (ewp-call
   'metaweblog-upload-file
   address
   `(("name" . ,(file-name-nondirectory file))
     ("type" . ,(mailcap-file-name-to-mime-type file))
     ("bits" . ,(with-temp-buffer
		  (set-buffer-multibyte nil)
		  (insert-file-contents-literally file)
		  (ewp-possibly-rotate-buffer image)
		  (base64-encode-region (point-min) (point-max))
		  (buffer-string))))))

(defun ewp-transform-and-upload-images (address)
  "Look for local <img> and upload images from those to Wordpress."
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
	     result size)
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
		  (ewp-call
		   'metaweblog-upload-file address
		   `(("name" . ,(file-name-nondirectory file))
		     ("type" . ,(mailcap-file-name-to-mime-type file))
		     ("bits" . ,data))))
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
		  (ewp-call
		   'metaweblog-upload-file address
		   `(("name" . ,(format "%s.%s"
					(format-time-string "%F")
					(cadr (split-string mime-type "/"))))
		     ("type" . ,mime-type)
		     ("bits" . ,data))))
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
		    (insert (getf (cdr image) :data))
		    (ewp-possibly-rotate-buffer image)
		    (buffer-string)))
		 (content-type (ewp-content-type data)))
	    (setq result
		  (ewp-call
		   'metaweblog-upload-file address
		   `(("name" . ,(format "%s.%s"
					(format-time-string "%F")
					(cadr (split-string content-type "/"))))
		     ("type" . ,content-type)
		     ("bits" . ,(base64-encode-string data)))))
	    (setq size (ewp-image-size (create-image data nil t)))
	    ;; Remove the <a> that we slap around images.
	    (when (and link-start
		       link-end)
	      (setq start link-start
		    end link-end)))))
	  
	(when result
	  (let ((url (cdr (assoc "url" result)))
		(thumbnailp (get-text-property start 'ewp-thumbnail))
		factor)
	    (when (> (car size) ewp-image-width)
	      (setq factor (/ (* ewp-image-width 1.0) (car size))))
	    (when url
	      (delete-region start end)
	      (goto-char start)
	      (if (and ewp-embed-smaller-images
		       (string-match ewp-embed-smaller-images ewp-address))
		  (insert
		   (format
		    "<a href=\"%s\"><img src=\"%s\" alt=\"\" wp-image-%s /></a>"
		    url
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
		  "<a href=\"%s\"><img src=\"%s\" alt=\"\" width=\"%d\" height=\"%d\" class=\"alignnone size-full wp-image-%s\" /></a>"
		  url url
		  (if factor
		      ewp-image-width
		    (car size))
		  (if factor
		      (* (cdr size) factor)
		    (cdr size))
		  (cdr (assoc "id" result)))))
	      ;; Preserve the thumnail designation.
	      (when thumbnailp
		(put-text-property start (point)
				   'ewp-thumbnail thumbnailp)))))))))

(defun ewp-transform-and-upload-videos (address)
  "Look for local <video ...> and upload mp4s from those to Wordpress."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<video .*?src=\"\\([^\"]+\\)\"" nil t)
      (let* ((file (match-string 1))
	     (start (match-beginning 1))
	     (end (match-end 1))
	     (url (url-generic-parse-url file)))
	;; Local file.
	(when (null (url-type url))
	  (when-let* ((result
		       (ewp-call
			'metaweblog-upload-file address
			`(("name" . ,(file-name-nondirectory file))
			  ("type" . ,(mailcap-file-name-to-mime-type file))
			  ("bits" . ,(with-temp-buffer
				       (set-buffer-multibyte nil)
				       (insert-file-contents-literally file)
				       (base64-encode-region (point-min)
							     (point-max))
				       (buffer-string))))))
		      (url (cdr (assoc "url" result))))
	    (delete-region start end)
	    (goto-char start)
	    (insert url)))))))

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
		 (file (concat (make-temp-name "/tmp/ewp") ".png")))
	    ;; Local file.
	    (when (and (not (equal (url-host url) address))
		       (dom-attr dom 'screenshot)
		       (not (dom-attr dom 'onmouseenter)))
	      (call-process "cutycapt" nil nil nil
			    "--out-format=png"
			    (format "--url=%s" (dom-attr dom 'href))
			    (format "--out=%s" file))
	      (when (file-exists-p file)
		(when-let* ((result
			     (ewp-call
			      'metaweblog-upload-file address
			      `(("name" . ,(file-name-nondirectory file))
				("type" . ,(mailcap-file-name-to-mime-type
					    file))
				("bits" . ,(with-temp-buffer
					     (set-buffer-multibyte nil)
					     (insert-file-contents-literally
					      file)
					     (base64-encode-region (point-min)
								   (point-max))
					     (buffer-string))))))
			    (image-url (cdr (assoc "url" result))))
		  (delete-region start (point))
		  (dom-remove-attribute dom 'screenshot)
		  (dom-set-attribute dom 'data-cached-time
				     (format-time-string "%FT%T"))
		  (dom-set-attribute dom 'data-cached-image image-url)
		  (ewp-print-html dom t))))))))))

(defun dom-remove-attribute (node attribute)
  "Remove ATTRIBUTE from NODE."
  (setq node (dom-ensure-node node))
  (when-let ((old (assoc attribute (cadr node))))
    (setcar (cdr node) (delq old (cadr node)))))

(defun ewp-possibly-rotate-buffer (image)
  (when (and image
	     (consp image)
	     (eq (car image) 'image))
    (when (image-property image :rotation)
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
		   (cl-case (truncate (image-property image :rotation))
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
	   "-rotate" (format "%d" (image-property image :rotation))
	   "-" "-")
	  (when (and (equal content-type "image/jpeg")
		     (executable-find "exiftool"))
	    (call-process-region
	     (point-min) (point-max) "exiftool" t (list (current-buffer) nil) nil
	     "-Orientation#=0"
	     "-o" "-" "-"))))))
    (when (and (image-property image :width)
	       (executable-find "convert"))
      (call-process-region
       (point-min) (point-max) "convert" t (list (current-buffer) nil) nil
       "-resize" (format "%dx" (image-property image :width))
       "-" "-"))))

(defun ewp-insert-image-thumbnails ()
  "Insert thumbnails."
  (interactive)
  (ewp-remove-image-thumbnails)
  (ewp-update-images)
  (message "Inserting image thumbnails..."))

(defun ewp-remove-image-thumbnails ()
  "Remove thumbnails."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when-let* ((props (get-text-property (point) 'display)))
	  (when (and (consp props)
		     (eq (car props) 'image))
	    (put-text-property (point) (1+ (point)) 'display nil)))
	(forward-char 1)))))

(defun ewp-browse ()
  "Display the blog post under point with `eww'."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
    (unless data
      (error "No post under point"))
    (eww (or (cdr (assoc "short_url" data))
	     (cdr (assoc "link" data))))))

(defun ewp-preview ()
  "Preview the blog post under point."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
    (unless data
      (error "No post under point"))
    (funcall shr-external-browser
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
    (dolist (address ewp-blog-addresses)
      (insert (propertize address
			  'face 'variable-pitch
			  'data address)
	      "\n"))
    (goto-char (point-min))))

(defvar ewp-list-blogs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "\r" 'ewp-list-blog)
    (define-key map "g" 'ewp-blogs)
    map))

(define-derived-mode ewp-list-blogs-mode special-mode "ewp"
  "Major mode for listing Wordpress blogs.

All normal editing commands are switched off.
\\<ewp-list-blogs-mode-map>"
  (setq truncate-lines t))

(defun ewp-list-blog ()
  "List the blog under point."
  (interactive)
  (let ((blog (get-text-property (point) 'data)))
    (unless blog
      (error "No blog under point"))
    (ewp-blog blog)))

(defun ewp-complete ()
  "Complete categories in that header."
  (interactive)
  (cond
   ((let ((completion-fail-discreetly t))
      (completion-at-point))
    ;; Completion was performed; nothing else to do.
    nil)
   (t (indent-relative))))

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

(defun ewp-categories ()
  (if (boundp 'ewp-categories)
      ewp-categories
    (setq-local ewp-categories
		(loop for elem in (ewp-call 'metaweblog-get-categories
					    ewp-address)
		      collect (cons (cdr (assoc "categoryName" elem))
				    (cdr (assoc "categoryId" elem)))))))

(defun ewp-yank-with-href ()
  "Yank the current kill ring item as an <a href>."
  (interactive)
  (set-mark (point))
  (insert (format "<a screenshot=true href=%S></a>"
                  (substring-no-properties (current-kill 0))))
  (forward-char -4))

(defun ewp-yank-with-blockquote (&optional clipboard)
  "Yank the current kill ring item as a <blockquote>.
If given a prefix, yank from the clipboard."
  (interactive "P")
  (set-mark (point))
  (when-let ((url (x-get-selection-internal 'PRIMARY 'text/x-moz-url-priv)))
    (insert (format "<a screenshot=true href=%S></a>:\n\n"
		    (ewp-decode-text-selection url))))
  (insert "<blockquote>\n")
  (if clipboard
      (insert (decode-coding-string (x-get-selection-internal
				     'CLIPBOARD 'text/plain\;charset=utf-8)
				    'utf-8))
    (insert (substring-no-properties (current-kill 0))))
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
  (insert-image (create-image file (ewp--image-type) nil
			      :max-width (ewp--display-width)
			      :max-height (/ (frame-pixel-height) 1.5))
		(format "<img src=%S>" file))
  (insert "\n\n"))

(defun ewp-insert-video (file)
  "Prompt for a file and insert a [video ...] shortcode."
  (interactive "fVideo file: ")
  (insert (format "<video autoplay loop muted controls><source src=%S type=\"video/mp4\"></video>\n\n"
		  file)))

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
		 :max-width (ewp--display-width)
		 :max-height (- (frame-pixel-height) 150))
   (format "<img src=\"data:%s;base64,%s\">"
	   (ewp-content-type image)
	   ;; Get a base64 version of the image.
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (insert image)
	     (base64-encode-region (point-min) (point-max) t)
	     (buffer-string)))))

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
  (let ((data (loop for type in '(PRIMARY CLIPBOARD)
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
	 (loop for type in '(PRIMARY CLIPBOARD)
	       for st = (loop for st across
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

(defun ewp-list-media (&optional address old-media)
  "List the media on the ADDRESS blog."  
  (interactive)
  (let* ((address (or address ewp-address))
	 (media (nconc old-media
		       (ewp-call 'ewp-get-media-library address 500
				 (length old-media))))
	 marks data lines)
    (switch-to-buffer (format "*%s media*" address))
    (setq marks (and (boundp 'ewp-marks)
		     ewp-marks))
    (ewp-save-excursion
      (let ((inhibit-read-only t))
	(erase-buffer)
	(ewp-list-media-mode)
	(setq ewp-marks marks)
	(setq-local ewp-address address)
	(dolist (elem media)
	  (push
	   (list
	    (if (ewp-find-mark (cdr (assoc "attachment_id" elem)))
		"*"
	      " ")
	    (propertize
	     (format-time-string "%Y-%m-%d"
				 (caddr (assoc "date_created_gmt" elem)))
	     'face '(variable-pitch :foreground "#a0a0a0"))
	    (propertize
	     (cdr (assoc "type" elem))
	     'face '(variable-pitch :foreground "#808080"))
	    (propertize (cdr (assoc "title" elem))
			'face 'variable-pitch))
	   lines)
	  (push elem data))
	(variable-pitch-table '((:name "" :width 1)
				(:name "Date" :width 10)
				(:name "Type" :width 20)
				(:name "Name"))
			      (nreverse lines)
			      (nreverse data))))))

(defun ewp-load-more-media ()
  "Load more media from the blog."
  (interactive)
  (ewp-list-media ewp-address (ewp-current-data)))

(defvar ewp-list-media-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'ewp-list-media)
    (define-key map "w" 'ewp-copy-media)
    (define-key map "u" 'ewp-copy-url)
    (define-key map "m" 'ewp-upload-media)
    (define-key map "\r" 'ewp-show-media)
    (define-key map "n" 'ewp-show-media-goto-next)
    (define-key map " " 'ewp-toggle-media-mark)
    (define-key map ">" 'ewp-load-more-media)
    map))

(define-derived-mode ewp-list-media-mode special-mode "ewp"
  "Major mode for listing Wordpress media.

All normal editing commands are switched off.
\\<ewp-list-media-mode-map>"
  (setq truncate-lines t)
  (setq-local ewp-marks nil))

(defun ewp-upload-media (file)
  "Upload media files to Wordpress."
  (interactive "fFile to upload: ")
  (let ((result (ewp-upload-file ewp-address file)))
    (message "Uploaded %s to %s (copied to clipboard)"
	     file ewp-address)
    (with-temp-buffer
      (insert (cdr (assoc "url" result)))
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
  (let* ((data (get-text-property (point) 'data))
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

(defun ewp-copy-media ()
  "Copy the media under point to the kill ring."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
    (if (not (or ewp-marks data))
	(error "No media under point")
      (ewp-copy-media-1 (or ewp-marks (list data))
			(length (or ewp-marks (list data)))))))

(defun ewp-copy-media-1 (elems length &optional prev)
  (let ((url (cdr (assoc "link" (pop elems)))))
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
	     (insert-image (create-image image (ewp--image-type) t
					 :max-width 500)
			   (format "<a href=%S><img src=%S></a>\n" url url))
	     (insert "\n\n")
	     (if elems
		 (ewp-copy-media-1 elems length (buffer-string))
	       (copy-region-as-kill (point-min) (point-max))
	       (message "Copied %s image%s to the kill ring"
			length (if (= length 1) "" "s"))))))))))

(defun ewp-copy-url ()
  "Copy the URL under point to the kill ring."
  (interactive)
  (let* ((data (get-text-property (point) 'data))
	 (url (cdr (assoc "link" data))))
    (if (not data)
	(error "No media under point")
      (with-temp-buffer
	(insert url)
	(copy-region-as-kill (point-min) (point-max))
	(message "Copied %s to the kill ring" url)))))

(defun ewp-import-screenshot (delay)
  "Take a screenshot and insert in the current buffer.
DELAY (the numeric prefix) says how many seconds to wait before
starting the screenshotting process."
  (interactive "p")
  (unless (executable-find "import")
    (error "Can't find ImageMagick import command on this system"))
  (decf delay)
  (unless (zerop delay)
    (dotimes (i delay)
      (message "Sleeping %d second%s..."
	       (- delay i)
	       (if (= (- delay i) 1)
		   ""
		 "s"))
      (sleep-for 1)))
  (message "Take screenshot")
  (let ((image
	 (with-temp-buffer
	   (set-buffer-multibyte nil)
	   (call-process "import" nil (current-buffer) nil "png:-")
	   (buffer-string))))
    (set-mark (point))
    (ewp-insert-image-data image)
    (insert "\n\n")
    (message "")))

(defun ewp--identify (image)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if (plist-get (cdr image) :file)
	(insert-file-contents-literally (plist-get (cdr image) :file))
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
  (let ((data (get-text-property (point) 'data)))
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

(defun ewp-list-comments (&optional address old-data)
  "List the recent comments for the blog."
  (interactive)
  (let ((address (or address ewp-address))
	lines data)
    (switch-to-buffer (format "*%s comments*" address))
    (ewp-list-comments-mode)
    (setq-local ewp-address address)
    (ewp-save-excursion
      (let ((inhibit-read-only t))
	(erase-buffer)
	(dolist (comment (nconc old-data
				(ewp-call 'ewp-get-comments address 100
					  (length old-data))))
	  (push comment data)
	  (push (ewp-print-comment comment) lines))
	(variable-pitch-table '((:name "Date")
				(:name "Status" :width 10)
				(:name "Author" :width 10)
				(:name "Title" :width 15)
				(:name "Comment" :width 100))
			      (nreverse lines)
			      (nreverse data))
	(goto-char (point-min))
	(forward-line 1)))))

(defun ewp-load-more-comments ()
  "Load more comments from the blog."
  (interactive)
  (ewp-list-comments nil (ewp-current-data)))

(defun ewp-current-data ()
  (let ((data nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when-let ((elem (get-text-property (point) 'data)))
	  (push elem data))
	(forward-line 1)))
    (nreverse data)))

(defvar ewp-list-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'ewp-list-comments)
    (define-key map "\r" 'ewp-display-comment)
    (define-key map "a" 'ewp-approve-comment)
    (define-key map "h" 'ewp-hold-comment)
    (define-key map "d" 'ewp-trash-comment)
    (define-key map "u" 'ewp-undelete-comment)
    (define-key map "r" 'ewp-make-comment)
    (define-key map "e" 'ewp-make-comment-edit)
    (define-key map ">" 'ewp-load-more-comments)
    map))

(define-derived-mode ewp-list-comments-mode special-mode "ewp"
  "Major mode for listing Wordpress comments.

All normal editing commands are switched off.
\\<ewp-list-comments-mode-map>"
  (buffer-disable-undo)
  (setq-local ewp-deleted-comments nil)
  (setq truncate-lines t))

(defun ewp-print-comment (comment)
  "Insert a Wordpress entry at point."
  (list
   (propertize
    (format-time-string "%Y-%m-%d"
			(caddr (assoc "date_created_gmt" comment)))
    'face 'variable-pitch)
   (propertize (cdr (assoc "status" comment))
	       'face '(variable-pitch :foreground "#a0a0a0"))
   (propertize
    (mm-url-decode-entities-string (or (cdr (assoc "author" comment)) ""))
    'face `(variable-pitch
	    :foreground "#b0b0b0"
	    ,@(if (not (equal (cdr (assoc "type" comment))
			      "pingback"))
		  (list :background "#505050"))))
   (propertize
    (mm-url-decode-entities-string (cdr (assoc "post_title" comment)))
    'face '(variable-pitch :foreground "#b0b0b0"))
   (propertize
    (mm-url-decode-entities-string
     (replace-regexp-in-string "[\n ]+" " " (cdr (assoc "content" comment))))
    'face 'variable-pitch)))

(defun ewp-display-comment ()
  "Display the comment under point."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
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
  (let ((data (get-text-property (point) 'data)))
    (unless data
      (error "No comment under point"))
    (setcdr (assoc "status" data) status)
    (let ((result (ewp-call 'ewp-edit-comment ewp-address
			    (cdr (assoc "comment_id" data)) data))
	  (inhibit-read-only t))
      (if (not (eq result t))
	  (message "Got an error: %s" result)
	(message "Updated comment successfully")
	(ewp-update-field status 2)))))

(defun ewp-update-field (string field)
  (save-excursion
    (beginning-of-line)
    (let ((count 1))
      (while (and (not (eolp))
		  (< count field))
	(forward-char 1)
	(let ((prop (get-text-property (point) 'display)))
	  (when (and prop
		     (consp prop)
		     (eq (car prop) 'space))
	    (incf count))))
      (forward-char 1)
      (let ((start (point))
	    (face (get-text-property (point) 'face)))
	(while (not (get-text-property (point) 'display))
	  (forward-char))
	(delete-region start (point))
	(insert string)
	(put-text-property start (point) 'face face)
	(put-text-property start (point) 'vtp-value string)))))

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
  (let ((data (get-text-property (point) 'data)))
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
	 (data (get-text-property 1 'data line))
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
  (let ((data (get-text-property (point) 'data))
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
	   (getf auth :user) (funcall (getf auth :secret))
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
  (let* ((data (get-text-property (point) 'data))
	 (id (cdr (assoc "attachment_id" data)))
	 (inhibit-read-only t))
    (unless data
      (error "No comment under point"))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (insert
       (propertize
	(if (ewp-find-mark id)
	    (progn
	      (setq ewp-marks (delq (ewp-find-mark id) ewp-marks))
	      " ")
	  (push data ewp-marks)
	  "*")
	'data data)))))

(defun ewp-find-mark (id)
  (loop for elem in ewp-marks
	when (equal (cdr (assoc "attachment_id" elem)) id)
	return elem))

(defun ewp-crop-image ()
  "Crop the image under point."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    ;; We replace the image under point with an SVG image that looks
    ;; just like that image.  That allows us to draw lines over it.
    ;; At the end, we replace that SVG with a cropped version of the
    ;; original image.
    (let* ((data (getf (cdr image) :data))
	   (undo-handle (prepare-change-group))
	   (orig-data data)
	   (type (cond
		  ((getf (cdr image) :format)
		   (format "%s" (getf (cdr image) :format)))
		  (data
		   (ewp-content-type data))))
	   (image-scaling-factor 1)
	   (size (image-size image t))
	   (svg (svg-create (car size) (cdr size)
			    :xmlns:xlink "http://www.w3.org/1999/xlink"
			    :stroke-width 5))
	   (text (buffer-substring (line-beginning-position)
				   (line-end-position)))
	   (inhibit-read-only t))
      (when (null data)
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally (getf (cdr image) :file))
	  (setq orig-data (buffer-string))
	  (setq type (ewp-content-type orig-data))
	  (call-process-region (point-min) (point-max)
			       "convert" t (current-buffer) nil
			       "-resize" "600x"
			       "-"
			       (format "%s:-" (cadr (split-string type "/"))))
	  (setq data (buffer-string))))
      (svg-embed svg data type t
		 :width (car size)
		 :height (cdr size))
      (delete-region (line-beginning-position)
		     (line-end-position))
      (svg-insert-image svg)
      (let ((area (condition-case _
		      (save-excursion
			(forward-line 1)
			(ewp-crop-image-1 svg))
		    (quit nil))))
	(delete-region (line-beginning-position) (line-end-position))
	(if area
	    (ewp-crop-image-update area orig-data size type)
	  ;; If the user didn't complete the crop, re-insert the
	  ;; original image (and text).
	  (insert text))
	(undo-amalgamate-change-group undo-handle)))))

(defun ewp-crop-image-update (area data size type)
  (let* ((image-scaling-factor 1)
	 (osize (image-size (create-image data (ewp--image-type) t) t))
	 (factor (/ (float (car osize)) (car size))))
    (ewp-insert-image-data
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert data)
       (call-process-region
	(point-min) (point-max) "convert"
	t (list (current-buffer) nil) nil
	"+repage" "-crop"
	(format
	 ;; width x height + left + top
	 "%dx%d+%d+%d"
	 (abs (truncate (* factor (- (getf area :right) (getf area :left)))))
	 (abs (truncate (* factor (- (getf area :bottom) (getf area :top)))))
	 (truncate (* factor (min (getf area :left) (getf area :right))))
	 (truncate (* factor (min (getf area :top) (getf area :bottom)))))
	"-" (format "%s:-" (cadr (split-string type "/"))))
       (buffer-string)))))
      
(defun ewp-crop-image-1 (svg)
  (track-mouse
    (loop with prompt = "Set start point"
	  and state = 'begin
	  and area = (list :left 0
			   :top 0
			   :right 0
			   :bottom 0)
	  and corner = nil
	  for event = (read-event prompt)
	  do (if (or (not (consp event))
		     (not (nth 7 (cadr event)))
		     ;; Only do things if point is over the SVG being
		     ;; tracked.
		     (not (eq (getf (cdr (nth 7 (cadr event))) :type) 'svg)))
		 ()
	       (let ((pos (nth 8 (cadr event))))
		 (cl-case state
		   ('begin
		    (cond
		     ((eq (car event) 'down-mouse-1)
		      (setq state 'stretch
			    prompt "Stretch to end point")
		      (setf (getf area :left) (car pos)
			    (getf area :top) (cdr pos)
			    (getf area :right) (car pos)
			    (getf area :bottom) (cdr pos)))))
		   ('stretch
		    (cond
		     ((eq (car event) 'mouse-movement)
		      (setf (getf area :right) (car pos)
			    (getf area :bottom) (cdr pos)))
		     ((memq (car event) '(mouse-1 drag-mouse-1))
		      (setq state 'corner
			    prompt "Choose corner to adjust (RET to crop)"))))
		   ('corner
		    (cond
		     ((eq (car event) 'down-mouse-1)
		      ;; Find out what corner we're close to.
		      (setq corner (ewp-find-corner
				    area pos
				    '((:left :top)
				      (:left :bottom)
				      (:right :top)
				      (:right :bottom))))
		      (when corner
			(setq state 'adjust
			      prompt "Adjust crop")))))
		   ('adjust
		    (cond
		     ((memq (car event) '(mouse drag-mouse-1))
		      (setq state 'corner
			    prompt "Choose corner to adjust"))
		     ((eq (car event) 'mouse-movement)
		      (setf (getf area (car corner)) (car pos)
			    (getf area (cadr corner)) (cdr pos))))))))
	  do (svg-line svg (getf area :left) (getf area :top)
		       (getf area :right) (getf area :top)
		       :id "top-line" :stroke-color "white")
	  (svg-line svg (getf area :left) (getf area :bottom)
		    (getf area :right) (getf area :bottom)
		    :id "bottom-line" :stroke-color "white")
	  (svg-line svg (getf area :left) (getf area :top)
		    (getf area :left) (getf area :bottom)
		    :id "left-line" :stroke-color "white")
	  (svg-line svg (getf area :right) (getf area :top)
		    (getf area :right) (getf area :bottom)
		    :id "right-line" :stroke-color "white")
	  while (not (member event '(return ?q)))
	  finally (return (and (eq event 'return)
			       area)))))

(defun ewp-find-corner (area pos corners)
  (loop for corner in corners
	;; We accept 10 pixels off.
	when (and (< (- (car pos) 10)
		     (getf area (car corner))
		     (+ (car pos) 10))
		  (< (- (cdr pos) 10)
		     (getf area (cadr corner))
		     (+ (cdr pos) 10)))
	return corner))

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
    (let* ((data (getf (cdr image) :data))
	   (inhibit-read-only t))
      (when (null data)
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert-file-contents-literally (getf (cdr image) :file))
	  (setq data (buffer-string)))
	(setq type (ewp-content-type data)))
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

(defun ewp-delete-post (blog-xmlrpc user-name password _blog-id post-id)
  "Delete an entry from the weblog system."
  (xml-rpc-method-call blog-xmlrpc
                       "blogger.deletePost"
                       nil
                       post-id
                       user-name
                       password
                       t))

(defun ewp-trash-post ()
  "Trash (i.e., delete) the post under point."
  (interactive)
  (let ((data (get-text-property (point) 'data))
	(inhibit-read-only t))
    (unless data
      (error "No post under point"))
    (when (yes-or-no-p "Really delete this post? ")
      (let* ((auth (ewp-auth ewp-address))
	     (all-data (metaweblog-get-post
			(ewp-xmlrpc-url ewp-address)
			(getf auth :user) (funcall (getf auth :secret))
			(cdr (assoc "post_id" data)))))
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
	 (result
	  (ewp-call 'metaweblog-edit-post ewp-address
		    (cdr (assoc "post_id" data))
		    data)))
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
    (setf (getf (cdr image) :width) width)))

(defun ewp-get-post-data (category)
  (loop for elem in (ewp-call 'ewp-get-posts ewp-address 300 0 nil
			      ["post_title" "post_date" "post_status" "terms"
			       "link" "post_name" "post_content"])
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
		    (ewp-call
		     'metaweblog-upload-file address
		     `(("name" . ,file)
		       ("type" . ,(mailcap-file-name-to-mime-type file))
		       ("bits" . ,(buffer-substring (point) (point-max)))
		       ("date" . ,date)))))
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

(defun dom-print (dom &optional pretty xml)
  "Print DOM at point as HTML/XML.
If PRETTY, indent the HTML/XML logically.
If XML, generate XML instead of HTML."
  (let ((column (current-column)))
    (insert (format "<%s" (dom-tag dom)))
    (let* ((attr (dom-attributes dom))
	   (column (1+ (current-column))))
      (dolist (elem attr)
	;; In HTML, these are boolean attributes that should not have
	;; an = value.
	(if (and (memq (car elem)
		       '(async autofocus autoplay checked
			       contenteditable controls default
			       defer disabled formNoValidate frameborder
			       hidden ismap itemscope loop
			       multiple muted nomodule novalidate open
			       readonly required reversed
			       scoped selected typemustmatch))
		 (cdr elem)
		 (not xml))
	    (insert (format " %s" (car elem)))
	  (insert (format " %s=%S" (car elem) (cdr elem))))))
    (let* ((children (dom-children dom))
	   (times (length children))
	   (non-text nil))
      (if (null children)
	  (insert " />")
	(insert ">")
        (dolist (child children)
	  (if (stringp child)
	      (insert child)
	    (setq non-text t)
	    (when pretty
              (insert "\n" (make-string (+ column 2) ? )))
	    (dom-print child pretty xml)))
	;; If we inserted non-text child nodes, or a text node that
	;; ends with a newline, then we indent the end tag.
        (when (and pretty
		   (or (bolp)
		       non-text))
	  (unless (bolp)
            (insert "\n"))
	  (insert (make-string column ? )))
        (insert (format "</%s>" (dom-tag dom)))))))

(provide 'ewp)

;;; ewp.el ends here
