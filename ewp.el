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

;; # apt install exiftool
;; if you want images to be properly rotated.

;; `M-x ewp' to get started.

;; If you have several blogs,

;; (setq ewp-blog-addresses '("my.example.com" "other.foo.bar"))
;;
;; and start with `M-x ewp-blogs'.

;;; Code:

(require 'cl)
(require 'metaweblog)
(require 'mm-url)

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

(defvar ewp-post)
(defvar ewp-address)
(defvar ewp-categories)

(defvar ewp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "e" 'ewp-select-post)
    (define-key map "p" 'ewp-preview)
    (define-key map "n" 'ewp-new-post)
    (define-key map "N" 'ewp-new-page)
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

(defun ewp (&optional address)
  "List all the posts on the blog."
  (interactive (list (cond
		      ((and (boundp 'ewp-address)
			    ewp-address)
		       ewp-address)
		      (ewp-blog-address)
		      (t
		       (read-string "Blog address (eg. my.example.com): "
				    nil 'ewp-history)))))
  (switch-to-buffer (format "*%s posts*" address))
  (let* ((auth (ewp-auth address))
	 (inhibit-read-only t))
    (erase-buffer)
    (ewp-list-mode)
    (setq-local ewp-address address)
    (dolist (post
	     (ewp-get-posts
	      (format "https://%s/xmlrpc.php" address)
	      (getf auth :user) (funcall (getf auth :secret))
	      ewp-blog-id 1000))
      (ewp-print-entry post "post"))
    (dolist (post
	     (wp-get-pagelist
	      (format "https://%s/xmlrpc.php" address)
	      (getf auth :user) (funcall (getf auth :secret))
	      ewp-blog-id))
      (ewp-print-entry post "page"))
    (goto-char (point-min))))

(defun ewp-limit-string (string length)
  (if (< (length string) length)
      string
    (substring string 0 length)))

(defun ewp-print-entry (post prefix)
  "Insert a Wordpress entry at point."
  (insert
   (propertize
    (format
     "%s %s%s%s%s%s\n"
     (propertize
      (format-time-string "%Y-%m-%d"
			  (or (caddr (assoc "post_date" post))
			      (caddr (assoc "date_created_gmt" post))))
      'face 'variable-pitch)
     (propertize 
      (ewp-limit-string (or (cdr (assoc (format "%s_status" prefix) post)) "")
			10)
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
      (mm-url-decode-entities-string
       (cdr (assoc (format "%s_title" prefix) post)))
      'face 'variable-pitch))
    'data post)))

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

(defun ewp-get-posts (blog-xmlrpc user-name password blog-id posts)
  "Retrieves list of posts from the weblog system. Uses wp.getPages."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPosts"
                       blog-id
                       user-name
                       password
		       `(("number" . ,posts))
		       ["post_title" "post_date" "post_status" "terms"]))

(defun ewp-get-page (blog-xmlrpc user-name password page-id)
  "Retrieves a page from the weblog. PAGE-ID is the id of the post
which is to be returned.  Can be used with pages as well."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPage"
		       nil
                       page-id
                       user-name
                       password))

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
		(format "https://%s/xmlrpc.php" ewp-address)
		(getf auth :user) (funcall (getf auth :secret))
		id))
	 (address ewp-address))
    (switch-to-buffer (format "*%s edit*" id))
    (erase-buffer)
    (ewp-edit-mode)
    (setq-local ewp-address address)
    (insert "Title: " (cdr (assoc "title" post)) "\n")
    (insert "Categories: " (mapconcat 'identity (cdr (assoc "categories" post))
				      ",")
	    "\n")
    (insert "Status: " (cdr (assoc (if pagep
				       "page_status"
				     "post_status")
				   post))
	    "\n")
    (insert "\n")
    (insert (cdr (assoc "description" post)))
    (goto-char (point-min))
    (ewp-update-images)
    (ewp-save-buffer id)
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
       (mm-url-decode-entities-string url)
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
    (define-key map "\C-c\C-a" 'ewp-yank-with-href)
    (define-key map "\C-c\C-q" 'ewp-yank-with-blockquote)
    (define-key map "\C-c\C-h" 'ewp-yank-html)
    (define-key map "\C-c\C-p" 'ewp-yank-picture)
    (define-key map "\C-c\C-b" 'ewp-insert-bold)
    (define-key map "\C-c\C-i" 'ewp-insert-img)
    (define-key map "\C-c\C-d" 'ewp-download-and-insert-image)
    (define-key map "\C-c\C-t" 'ewp-insert-tag)
    (define-key map "\C-c\C-u" 'ewp-unfill-paragraph)
    (define-key map "\C-c\C-I" 'ewp-remove-image-thumbnails)
    (define-key map "\C-c\C-l" 'ewp-remove-html-layer)
    (define-key map "\t" 'ewp-complete)
    map))

(define-derived-mode ewp-edit-mode text-mode "ewp"
  "Major mode for editing Wordpress posts.
\\<ewp-mode-map>"
  (setq-local word-wrap t)
  (setq-local normal-auto-fill-function 'ignore)
  (setq-local completion-at-point-functions
	      (cons 'ewp-complete-category completion-at-point-functions)))

(defun ewp-update-post ()
  "Update the post in the current buffer on Wordpress."
  (interactive)
  (run-hooks 'ewp-send-hook)
  (save-buffer)
  (ewp-transform-and-upload ewp-address)
  (save-excursion
    (goto-char (point-min))
    (let ((headers nil)
	  (post (copy-list (or ewp-post
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
	      (mapcar #'string-trim
		      (split-string (cdr (assoc "Categories" headers)) ",")))
      (nconc post (list (cons "date" (ewp-current-time post))))
      (apply
       (if pagep
	   (if ewp-post
	       'wp-edit-page
	     'wp-new-page)
	 (if ewp-post
	     'metaweblog-edit-post
	   'metaweblog-new-post))
       `(,(format "https://%s/xmlrpc.php" ewp-address)
	 ,(getf auth :user)
	 ,(funcall (getf auth :secret))
	 ,@(if pagep
	       (list (format "%s" ewp-blog-id))
	     nil)
	 ,(format "%s" (if pagep
			   (cdr (assoc "page_id" post))
			 (cdr (assoc "postid" post))))
	 ,post
	 ;; Publish if already published.
	 ,(equal (cdr (assoc "Status" headers)) "publish")))
      (set-buffer-modified-p nil)
      (message "%s the post"
	       (if ewp-post
		   "Edited"
		 "Posted"))
      (bury-buffer))))

(defun ewp-current-time (post)
  (format-time-string
   "%Y%m%dT%H:%M:%S"
   (caddr (assoc "dateCreated_gmt" post))
   "UTC"))

(defun ewp-new-post ()
  "Start editing a new post."
  (interactive)
  (let ((address ewp-address))
    (switch-to-buffer (generate-new-buffer "*Wordpress Post*"))
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

(defun ewp-upload-media (address file &optional image)
  (let ((auth (ewp-auth address)))
    (metaweblog-upload-file
     (format "https://%s/xmlrpc.php" address)
     (getf auth :user) (funcall (getf auth :secret))
     (format "%s" ewp-blog-id)
     `(("name" . ,(file-name-nondirectory file))
       ("type" . ,(mailcap-file-name-to-mime-type file))
       ("bits" . ,(with-temp-buffer
		    (set-buffer-multibyte nil)
		    (ewp-possibly-rotate-image file image)
		    (base64-encode-region (point-min) (point-max))
		    (buffer-string)))))))

(defun ewp-transform-and-upload (address)
  "Look for local <img> and upload images from those to Wordpress."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<img.*?src=\"" nil t)
      (let* ((start (match-beginning 0))
	     (file (buffer-substring (point)
				     (progn
				       (re-search-forward "\".*>" nil t)
				       (match-beginning 0))))
	     (end (point))
	     ;; We're avoiding `url-generic-parse-url' and other
	     ;; regepx-based parsers here because data: URLs can be
	     ;; huge and blows up the regexp parser.
	     (type (and (string-match "^[^:]+" file)
			(match-string 0 file)))
	     result size)
	(cond
	 ;; Local file.
	 ((null type)
	  (setq result (ewp-upload-media
			address file (get-text-property start 'display))
		size (image-size (create-image file) t)))
	 ;; data: URL where the image is in the src bit.
	 ((and (equal type "data")
	       (string-match "^data:\\([^;]+\\);base64," file))
	  (let ((mime-type (match-string 1 file))
		(data (with-temp-buffer
			(insert file)
			(goto-char (point-min))
			(search-forward ",")
			(buffer-substring (point) (point-max)))))
	    (setq result
		  (let ((auth (ewp-auth address)))
		    (metaweblog-upload-file
		     (format "https://%s/xmlrpc.php" address)
		     (getf auth :user) (funcall (getf auth :secret))
		     (format "%s" ewp-blog-id)
		     `(("name" . ,(format "%s.%s" address
					  (cadr (split-string mime-type "/"))))
		       ("type" . ,mime-type)
		       ("bits" . ,data)))))
	    (setq size (image-size (create-image
				    (with-temp-buffer
				      (set-buffer-multibyte nil)
				      (insert data)
				      (base64-decode-region
				       (point-min) (point-max))
				      (buffer-string))
				    'imagemagick t))))))
	(when result
	  (let* ((url (cdr (assoc "url" result)))
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
    (eww (cdr (assoc "short_url" data)))))

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
    (insert-image (create-image file 'imagemagick nil
				:max-width 700)
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

(defun ewp-blogs ()
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
\\<ewp-mode-map>"
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun ewp-list-blog ()
  "List the blog under point."
  (interactive)
  (let ((blog (get-text-property (point) 'data)))
    (unless blog
      (error "No blog under point"))
    (ewp blog)))

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
	 (let ((categories (ewp-categories))
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

(defun ewp-categories ()
  (if (boundp 'ewp-categories)
      ewp-categories
    (let* ((auth (ewp-auth ewp-address))
	   (categories
	    (loop for elem in
		  (metaweblog-get-categories
		   (format "https://%s/xmlrpc.php" ewp-address)
		   (getf auth :user) (funcall (getf auth :secret))
		   ewp-blog-id)
		  collect (cdr (assoc "categoryName" elem)))))
      (setq-local ewp-categories categories)
      categories)))

(defun ewp-yank-with-href ()
  "Yank the current kill ring item as an <a href>."
  (interactive)
  (set-mark (point))
  (insert (format "<a href=%S></a>"
                  (substring-no-properties (current-kill 0))))
  (forward-char -4))

(defun ewp-yank-with-blockquote ()
  "Yank the current kill ring item as a <blockquote>."
  (interactive)
  (set-mark (point))
  (insert "<blockquote>\n")
  (insert (substring-no-properties (current-kill 0)))
  (insert "</blockquote>\n\n"))

(defun ewp-insert-img (file)
  "Prompt for a file and insert an <img>."
  (interactive "fImage file: ")
  (insert-image (create-image file 'imagemagick nil :max-width 500)
		(format "<img src=%S>" file))
  (insert "\n\n"))

(defun ewp-insert-bold ()
  "Insert <b> tags."
  (interactive)
  (insert "<b></b>\n")
  (backward-char 5))

(defun ewp-insert-tag (tag)
  "Insert a balanced pair of tags."
  (interactive (list (completing-read
		      "Tag: "
		      '("b" "blockquote" "body" "div" "em"
			"h1" "h2" "h3" "h4" "h5" "h6"
			"i" "img" "ul" "li" "ol" "pre" "span"
			"table" "td" "tr" "u"))))
  (insert "<" tag ">")
  (let ((point (point)))
    (insert "</" tag ">")
    (goto-char point)))

(defun ewp-unfill-paragraph ()
  "Remove newlines from the current paragraph."
  (interactive)
  (save-excursion
    (let ((start (progn (forward-paragraph -1) (point)))
	  (end (progn (forward-paragraph 1) (point))))
      (goto-char start)
      (unless (bobp)
	(forward-line 1))
      (while (re-search-forward " *\n *" end t)
	(replace-match " " t t)))))

(defun ewp-download-and-insert-image ()
  "Download and insert the image from the kill ring."
  (interactive)
  (let* ((url (substring-no-properties (current-kill 0)))
	 (buffer (current-buffer)))
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
	       (goto-char (point-max))
	       (ewp-insert-image-data image)
	       (insert "\n\n")))))))))

(defun ewp-insert-image-data (image)
  (insert-image
   (create-image image 'imagemagick t
		 :max-width 500)
   (format "<img src=\"data:%s;base64,%s\">"
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (insert image)
	     (call-process-region (point-min) (point-max)
				  "file" t (current-buffer) nil
				  "--mime-type" "-")
	     (cadr (split-string (buffer-string))))
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (insert image)
	     (base64-encode-region (point-min) (point-max) t)
	     (buffer-string)))))

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
      (insert
       (if (and (> (length data) 2)
		(= (aref data 0) 255)
		(= (aref data 1) 254))
	   ;; Somehow the selection is UTF-16 when selecting text in
	   ;; Firefox.
	   (decode-coding-string data 'utf-16-le)
	 ;; But some sources add a nul to the end of the data.
	 (replace-regexp-in-string (string 0) "" data))))))

(defun ewp-yank-picture ()
  "Yank the contents of the current X image selection/clipboard, if any."
  (interactive)
  (let ((data
	 (loop for type in '(PRIMARY CLIPBOARD)
	       for data = (loop for image-type in '(image/jpeg
						    image/png
						    image/gif)
				for data = (x-get-selection-internal
					    type image-type)
				when data
				return data)
	       when data
	       return data)))
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

(provide 'ewp)

;;; ewp.el ends here
