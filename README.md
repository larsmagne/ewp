ewp is an Emacs package to post and edit posts and pages on blogging platforms like Wordpress.

See https://lars.ingebrigtsen.no/2018/10/02/editing-wordpress-articles-in-emacs/ for details.

# Requirements

* A newish version of Emacs, for instance Emacs 26 or newer, and the following two repositories:
* https://github.com/hexmode/xml-rpc-el
* https://github.com/org2blog/metaweblog

# Set Up

To get started, put something like the following in your .emacs:

```
   (cl-push "~/src/ewp" load-path)
   (autoload 'ewp-blogs "ewp.el" "List Wordpress blogs" t)
   (setq ewp-blog-addresses '("blog.example.org" "other.blog.com"))
```

and then `M-x ewp-blogs' to start browsing.

# Modes

There are three different modes made available by this package:

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


## ewp-edit-mode:

* C-c C-c: Post your edits to the blog.  This will update your Wordpress.
* C-c C-a: Treat the contents of the kill ring as an URL and insert it as &lt;a href="..."&gt;&lt;/a&gt;
* C-c C-q: Yank the contents of a kill ring into &lt;blockquote&gt;.
* C-c C-i: Insert an image into the buffer.
* C-c C-i: Prompt for an HTML tag and insert a &lt;tag&gt;&lt;/tag&gt; pair.
* TAB:     In the Categories header, provide category completion.

