ewp is an Emacs package to post and edit posts and pages on blogging platforms like Wordpress.

See https://lars.ingebrigtsen.no/2018/10/02/editing-wordpress-articles-in-emacs/ for details.

# Requirements

* A newish version of Emacs, for instance Emacs 26 or newer, and the following three repositories:
* https://github.com/hexmode/xml-rpc-el
* https://github.com/org2blog/metaweblog
* https://github.com/larsmagne/vpt.el

# Set Up

To get started, put something like the following in your .emacs:

```
   (push "~/src/ewp" load-path)
   (autoload 'ewp-blogs "ewp.el" "List Wordpress blogs" t)
   (setq ewp-blog-addresses '("blog.example.org" "other.blog.com"))
```

and then `M-x ewp-blogs' to start browsing.  You should be queried for the user name/password to use to access the blog.  If you want to store that permanently, put something like the following in your ~/.authinfo file:

```
  machine blog.example.org port https login foo password bar
```

# Modes

There are five different modes made available by this package:

## ewp-list-blogs-mode:

* RET: Select the blog under point.
* g: Regenerate based on `ewp-blog-addresses'


## ewp-list-mode:

* RET: Browse the post/page under point with eww.
* e: Edit the blog post/page under point.
* n: Create a new post.
* N: Create a new page.
* p: Preview the draft under point in an external web browser.
* g: Rescan the list of blog posts/pages on this blog.
* M: List the media on this blog.
* C: List the comments on this blog.
* c: Make a comment.
* s: List posts with a specific status.
* d: Delete the post under point.
* &gt;: Load more posts.


## ewp-edit-mode:

* C-c C-a: Treat the contents of the kill ring as an URL and insert it as &lt;a href="..."&gt;&lt;/a&gt;
* C-c C-y: Yank URL and text of the current selection as a link.
* C-c C-b: Yank the contents of a kill ring into &lt;blockquote&gt;.
* C-c C-c: Post your edits to the blog.  This will update your Wordpress.
* C-c C-d: Download the image in the kill ring and insert it.
* C-c C-i: Insert an image into the buffer.
* C-c C-l: Remove one layer of HTML tagging.
* C-c C-m: Yank the current text/html markup from the X selection.
* C-c C-n: Extract the <a>...</a> bit from the region.
* C-c C-o: Quote HTML entities in region.
* C-c C-p: Yank the current text/jpeg X selection.
* C-c C-q: Remove image thumbnails.
* C-c C-r: Prompt for an HTML tag and insert the pair around the region.
* C-c C-s: Import a screenshot.
* C-c C-t: Prompt for an HTML tag and insert a &lt;tag&gt;&lt;/tag&gt; pair.
* C-c C-u: Unfill a paragraph.
* C-c C-z: Schedule later posting.
* C-c C-k: Crop the image under point.
* C-c C-j: Set the width of the image under point.
* TAB:     In the Categories header, provide category completion.

## ewp-list-media-mode:

* RET: Look at the media item (i.e., image) under point
* SPACE: Toggle marking the item under point.
* n: Look at the image under point and goto next line
* w: Copy the image under point to the kill ring as an &lt;img&gt; construct
* u: Copy the URL under point to the kill ring.
* g: Rescan the media list.
* &gt;: Load more media.

## ewp-list-comments-mode:

* g: Rescan.
* a: Approve the comment under point.
* h: Hold (unapprove) the comment under point.
* d: Delete (trash) the comment under point.
* u: Undelete a previously deleted comment.
* r: Reply to a comment.
* RET: Display the comment under point.
* &gt;: Load more comments.

## Commands in Dired buffers

* `M-x ewp-dired-copy-as-kill'
* `M-x ewp-dired-upload-media'
