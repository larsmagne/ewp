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
(require 'dired)

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

(defvar ewp-html-tags
  '("b" "blockquote" "body" "div" "em" "h1" "h2" "h3" "h4" "h5" "h6"
    "i" "img" "ul" "li" "ol" "pre" "span" "table" "td" "tr" "u")
  "A list of HTML tags that you might want to complete over.")

(defvar ewp-post)
(defvar ewp-address)
(defvar ewp-categories)
(defvar ewp-comment)
(defvar ewp-edit)

(defvar ewp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "e" 'ewp-select-post)
    (define-key map "p" 'ewp-preview)
    (define-key map "m" 'ewp-list-media)
    (define-key map "n" 'ewp-new-post)
    (define-key map "N" 'ewp-new-page)
    (define-key map "g" 'ewp)
    (define-key map "\r" 'ewp-browse)
    (define-key map "w" 'ewp-copy-link)
    (define-key map "c" 'ewp-make-comment)
    (define-key map "C" 'ewp-list-comments)
    map))

(define-derived-mode ewp-list-mode special-mode "ewp"
  "Major mode for listing Wordpress posts.

All normal editing commands are switched off.
\\<ewp-list-mode-map>"
  (buffer-disable-undo)
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
  (let* ((inhibit-read-only t))
    (erase-buffer)
    (ewp-list-mode)
    (setq-local ewp-address address)
    (dolist (post (ewp-call 'ewp-get-posts address 100))
      (ewp-print-entry post "post"))
    (dolist (post (ewp-call 'wp-get-pagelist address))
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
  "Retrieves list of posts from the weblog system. Uses wp.getPosts."
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
    (define-key map "\C-c\C-a" 'ewp-yank-with-href)
    (define-key map "\C-c\C-b" 'ewp-yank-with-blockquote)
    (define-key map "\C-c\C-c" 'ewp-update-post)
    (define-key map "\C-c\C-d" 'ewp-download-and-insert-image)
    (define-key map "\C-c\C-i" 'ewp-insert-img)
    (define-key map "\C-c\C-l" 'ewp-remove-html-layer)
    (define-key map "\C-c\C-m" 'ewp-yank-html)
    (define-key map "\C-c\C-n" 'ewp-clean-link)
    (define-key map "\C-c\C-o" 'ewp-html-quote-region)
    (define-key map "\C-c\C-p" 'ewp-yank-picture)
    (define-key map "\C-c\C-q" 'ewp-remove-image-thumbnails)
    (define-key map "\C-c\C-r" 'ewp-tag-region)
    (define-key map "\C-c\C-s" 'ewp-import-screenshot)
    (define-key map "\C-c\C-t" 'ewp-insert-tag)
    (define-key map "\C-c\C-u" 'ewp-unfill-paragraph)
    (define-key map "\C-c\C-z" 'ewp-schedule)
    (define-key map "\t" 'ewp-complete)
    map))

(define-derived-mode ewp-edit-mode text-mode "ewp"
  "Major mode for editing Wordpress posts.
\\<ewp-mode-map>"
  (setq-local word-wrap t)
  (setq-local normal-auto-fill-function 'ignore)
  (setq-local completion-at-point-functions
	      (cons
	       'ewp-complete-status
	       (cons 'ewp-complete-category completion-at-point-functions))))

(defun ewp-update-post ()
  "Update the post in the current buffer on Wordpress."
  (interactive)
  (run-hooks 'ewp-send-hook)
  (when (buffer-file-name)
    (save-buffer))
  (ewp-transform-and-upload ewp-address)
  (if (and (boundp 'ewp-comment)
	   ewp-comment)
      (ewp-send-comment)
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
	(nconc post (list (cons "date" (ewp-current-time
					post
					(cdr (assoc "Schedule" headers))))))
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
	(bury-buffer)))))

(defun ewp-external-time (time)
  (format-time-string "%Y%m%dT%H:%M:%S" time "UTC"))

(defun ewp-current-time (post scheduled)
  (ewp-external-time
   (if (plusp (length scheduled))
       (parse-iso8601-time-string scheduled)
     (caddr (assoc "dateCreated_gmt" post)))))

(defun ewp-new-post (&optional address)
  "Start editing a new post."
  (interactive)
  (let ((address (or address ewp-address)))
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
  (ewp-call
   'metaweblog-upload-file
   address
   `(("name" . ,(file-name-nondirectory file))
     ("type" . ,(mailcap-file-name-to-mime-type file))
     ("bits" . ,(with-temp-buffer
		  (set-buffer-multibyte nil)
		  (ewp-possibly-rotate-image file image)
		  (base64-encode-region (point-min) (point-max))
		  (buffer-string))))))

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
	     (type (and (string-match "^[^:]+:" file)
			(substring file 0 (1- (match-end 0)))))
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
		  (ewp-call
		   'metaweblog-upload-file address
		   `(("name" . ,(format "%s.%s" address
					(cadr (split-string mime-type "/"))))
		     ("type" . ,mime-type)
		     ("bits" . ,data))))
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
		(loop for elem in (ewp-call 'metaweblog-get-categories address)
		      collect (cdr (assoc "categoryName" elem))))))

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
  (when-let ((url (x-get-selection-internal 'PRIMARY 'text/x-moz-url-priv)))
    (insert (format "On <a href=%S>somebody</a> wrote:\n\n"
		    (ewp-decode-text-selection url))))
  (insert "<blockquote>\n")
  (insert (substring-no-properties (current-kill 0)))
  (insert "</blockquote>\n\n"))

(defun ewp-insert-img (file)
  "Prompt for a file and insert an <img>."
  (interactive "fImage file: ")
  (insert-image (create-image file 'imagemagick nil :max-width 500)
		(format "<img src=%S>" file))
  (insert "\n\n"))

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
    (ewp-download-and-insert-image-1 url (current-buffer))))

(defun ewp-download-and-insert-image-1 (url buffer &optional callback)
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
	     (insert "\n\n"))
	   (when callback
	     (funcall callback))))))))

(defun ewp-insert-image-data (image)
  (insert-image
   (create-image image 'imagemagick t
		 :max-width 500)
   (format "<img src=\"data:%s;base64,%s\">"
	   ;; Get the MIME type by running "file" over it.
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (insert image)
	     (call-process-region (point-min) (point-max)
				  "file" t (current-buffer) nil
				  "--mime-type" "-")
	     (cadr (split-string (buffer-string))))
	   ;; Get a base64 version of the image.
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

(defun ewp-print-html (dom)
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
    (insert (format "</%s>" (car dom)))))

(defun ewp-get-media-library (blog-xmlrpc user-name password blog-id count)
  "Retrieves list of images etc from the weblog system. Uses wp.getMediaLibrary."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getMediaLibrary"
                       blog-id
                       user-name
                       password
		       `(("number" . ,count))))

(defun ewp-list-media (&optional address)
  "List the media on the ADDRESS blog."  
  (interactive)
  (let* ((address (or address ewp-address))
	 (media
	  (ewp-call 'ewp-get-media-library address 500)))
    (switch-to-buffer (format "*%s media*" address))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ewp-list-media-mode)
      (setq-local ewp-address address)
      (dolist (elem media)
	(insert
	 (propertize
	  (format
	   "%s %s %s\n"
	   (propertize
	    (format-time-string "%Y-%m-%d"
				(caddr (assoc "date_created_gmt" elem)))
	    'face '(variable-pitch :foreground "#a0a0a0"))
	   (propertize (cdr (assoc "title" elem))
		       'face 'variable-pitch)
	   (propertize
	    (cdr (assoc "type" elem))
	    'face '(variable-pitch :foreground "#808080")))
	  'data elem))))
    (goto-char (point-min))))

(defvar ewp-list-media-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'ewp-list-media)
    (define-key map "w" 'ewp-copy-media)
    (define-key map "\r" 'ewp-show-media)
    map))

(define-derived-mode ewp-list-media-mode special-mode "ewp"
  "Major mode for listing Wordpress media.

All normal editing commands are switched off.
\\<ewp-mode-map>"
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun ewp-show-media ()
  "Show the media under point."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
    (if (not data)
	(error "No media under point")
      (eww (cdr (assoc "link" data))))))

(defun ewp-copy-media ()
  "Copy the media under point to the kill ring."
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
	       (insert-image (create-image image 'imagemagick t
					   :max-width 500)
			     (format "<a href=%S><img src=%S></a>\n" url url))
	       (insert "\n")
	       (copy-region-as-kill (point-min) (point-max))
	       (message "Copied %s to the kill ring" url)))))))))

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
    (shr-probe-and-copy-url (cdr (assoc "short_url" data)))))

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

(defun ewp-get-comments (blog-xmlrpc user-name password blog-id comments)
  "Retrieves list of posts from the weblog system. Uses wp.getComments."
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getComments"
                       blog-id
                       user-name
                       password
		       `(("number" . ,comments))))

(defun ewp-list-comments (&optional address)
  "List the recent comments for the blog."
  (interactive)
  (let ((address (or address ewp-address)))
    (switch-to-buffer (format "*%s comments*" address))
    (ewp-list-comments-mode)
    (setq-local ewp-address address)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (comment (ewp-call 'ewp-get-comments address 100))
	(ewp-print-comment comment))
      (goto-char (point-min)))))

(defvar ewp-list-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'ewp-list-comments)
    (define-key map "\r" 'ewp-display-comment)
    (define-key map "a" 'ewp-approve-comment)
    (define-key map "h" 'ewp-hold-comment)
    (define-key map "d" 'ewp-trash-comment)
    (define-key map "r" 'ewp-make-comment)
    (define-key map "e" 'ewp-make-comment-edit)
    map))

(define-derived-mode ewp-list-comments-mode special-mode "ewp"
  "Major mode for listing Wordpress comments.

All normal editing commands are switched off.
\\<ewp-list-comments-mode-map>"
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun ewp-print-comment (comment)
  "Insert a Wordpress entry at point."
  (insert
   (propertize
    (format
     "%s %s%s%s%s%s%s %s\n"
     (propertize
      (format-time-string "%Y-%m-%d"
			  (caddr (assoc "date_created_gmt" comment)))
      'face 'variable-pitch)
     (propertize (cdr (assoc "status" comment))
		 'face '(variable-pitch :foreground "#a0a0a0"))
     (propertize " " 'display '(space :align-to 20))
     (propertize
      (ewp-limit-string (or (cdr (assoc "author" comment)) "") 15)
      'face `(variable-pitch :foreground "#b0b0b0"
			     ,@(if (not (equal (cdr (assoc "type" comment))
					       "pingback"))
				   (list :background "#505050"))))
     (propertize " " 'display '(space :align-to 35))
     (propertize
      (ewp-limit-string
       (mm-url-decode-entities-string (cdr (assoc "post_title" comment)))
       15)
      'face '(variable-pitch :foreground "#b0b0b0"))
     (propertize " " 'display '(space :align-to 50))
     (propertize
      (mm-url-decode-entities-string
       (replace-regexp-in-string "[\n ]+" " " (cdr (assoc "content" comment))))
      'face 'variable-pitch))
    'data comment)))

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
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

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
  (ewp-change-status "approve"))

(defun ewp-hold-comment ()
  "Unapprove the comment under point."
  (interactive)
  (ewp-change-status "hold"))

(defun ewp-change-status (status)
  (let ((data (get-text-property (point) 'data)))
    (unless data
      (error "No comment under point"))
    (setcdr (assoc "status" data) status)
    (let ((result
	   (ewp-call 'ewp-edit-comment ewp-address
		     (cdr (assoc "comment_id" data)) data)))
      (if (eq result t)
	  (message "Updated comment successfully")
	(message "Got an error: %s" result)))))

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
    (when (y-or-n-p "Really delete this comment?")
      (let ((result (ewp-call 'ewp-delete-comment ewp-address
			      (cdr (assoc "comment_id" data)))))
	(if (not (eq result t))
	    (message "Got an error: %s" result)
	  (message "Comment deleted")
	  (let ((inhibit-read-only t))
	    (delete-region (line-beginning-position)
			   (progn (forward-line 1) (point)))))))))

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

(defun ewp-make-comment-edit (&optional editp)
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
	   (format "https://%s/xmlrpc.php" address)
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
	   (car ewp-blog-addresses))))
    (dolist (file files)
      (ewp-upload-media address file))
    (message "Uploaded %s file%s to %s"
	     (length files)
	     (if (= (length files) 1)
		 ""
	       "s")
	     address)))

(provide 'ewp)

;;; ewp.el ends here
