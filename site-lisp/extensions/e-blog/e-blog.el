;;; e-blog.el --- a GNU Emacs interface to Blogger
;; Copyright (C)  2007 Mikey Coulson
;; Author: Mikey Coulson <miketcoulson@gmail.com>

;; This file is not part of GNU Emacs.

;; e-blog is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; e-blog is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with e-blog; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar e-blog-display-url nil
  "If non-nil, e-blog will display the post/edit url in post/edit
buffers.")
(defvar e-blog-link-region-key "\C-ch"
  "Default keybinding for inserting links in posts.")
(defvar e-blog-tt-region-key "\C-ct"
  "Default keybinding for inserting `tt' style tags.")
(defvar e-blog-emphasize-region-key "\C-ci"
  "Default keybinding for inserting `em' style tags.")
(defvar e-blog-strong-region-key "\C-cs"
  "Default keybinding for inserting `strong' style tags.")
  
(setq e-blog-name "eblog"
      e-blog-version "0.5"
      e-blog-service "blogger"
      e-blog-fetch-authinfo-url "https://www.google.com/accounts/ClientLogin"
      e-blog-fetch-bloglist-url "http://www.blogger.com/feeds/default/blogs"
      e-blog-post-url-rel "http://schemas.google.com/g/2005#post"
      e-blog-buffer "*e-blog*"
      e-blog-post-buffer "*e-blog post*"
      e-blog-choose-buffer "*e-blog choose*"
      e-blog-edit-buffer "*e-blog edit "
      e-blog-tmp-buffer "*e-blog tmp*"
      e-blog-auth nil)

(defface e-blog-url '((t :foreground "orange")) "Face used by e-blog for url line.")
(defface e-blog-title '((t :foreground "red")) "Face used by e-blog for title line.")
(defface e-blog-label '((t :foreground "green")) "Face used by e-blog for label line.")
(defface e-blog-post '((t :foreground "purple")) "Face used by e-blog for post-separator line.")
(defface e-blog-blog '((t (:foreground "cyan1" :underline t))) "Face used by e-blog for blog titles.")

(defun e-blog-get-credentials ()
  "Gets username and password via the minibuffer."
  (setq e-blog-user (read-from-minibuffer "Username: ")
	e-blog-passwd (read-passwd "Password: ")))

(defun e-blog-fetch-auth ()
  "Calls curl to request an authorization string for further
communication with the Gdata API."
  (e-blog-get-credentials)
  (let (switch common user pass source service)
    (setq switch "-d"
	  ampersand "\&"
	  user (concat "Email=" e-blog-user)
	  pass (concat "Passwd=" e-blog-passwd)
	  source (concat "source=" e-blog-name "-" e-blog-name "-" e-blog-version)
	  service (concat "service=" e-blog-service)
	  all (concat switch user ampersand pass ampersand
		      source ampersand service))
    (message "Sending authorization request...")
    (call-process "curl" nil e-blog-buffer nil
		  "--stderr" "/dev/null"
		  all e-blog-fetch-authinfo-url)))

(defun e-blog-check-authinfo ()
  (condition-case nil 
      (e-blog-extract-authinfo)
    (error
     (message "No authorization token was received.
Perhaps you mistyped your username or password."))))

(defun e-blog-extract-authinfo ()
  "Extracts the authorization token returned by Blogger and saves
it for future use."
  (set-buffer e-blog-buffer)
  (let (beg)
    (search-backward "Auth=")
    (setq beg (+ (point) 5))
    (move-end-of-line 1)
    (setq e-blog-auth
	  (concat
	   "Authorization: GoogleLogin auth="
	   (buffer-substring beg (point))))
    (erase-buffer)))

(defun e-blog-fetch-bloglist ()
  "Requests a list of blogs for `e-blog-user'."
  (let (feed)
    (set-buffer e-blog-buffer)
    (erase-buffer)
    (message "Requesting list of blogs...")
    (call-process "curl" nil e-blog-buffer nil
		  "--stderr" "/dev/null"
		  "--header"
		  e-blog-auth
		  e-blog-fetch-bloglist-url)
    (setq feed (buffer-substring (point-min) (point-max)))
    (message "Requesting list of blogs... Done.")
    feed))

(defun e-blog-kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun e-blog-forward-button ()
  "Moves point forward one button."
  (interactive)
  (forward-button 1 t))

(defun e-blog-setup-choose-buffer (feed)
  "Sets up the buffer that allows the user to choose which blog
to post to and optionally to list posts for each blog."
  (let (url)
    (set-buffer (get-buffer-create e-blog-choose-buffer))
    (erase-buffer)
    (insert-string
     (format "%d blogs found for %s:\n\n"
	     (length (e-blog-get-titles feed)) e-blog-user))
    (dolist (title (e-blog-get-titles feed))
      (insert-string "\t")
      (insert-text-button
       "+"
       'action 'e-blog-list-posts
       'face 'e-blog-label
       'title title
       'feed feed)
      (insert " ")
      (setq url (e-blog-get-post-url title feed))
      (insert-text-button
       title
       'action 'e-blog-setup-post-buffer
       'face 'e-blog-blog
       'url url)
      (insert-string "\n"))
    (insert-string "\nSelect which blog you would like to post to.")
    (local-set-key "\t" 'e-blog-forward-button)
    (local-set-key "q" 'e-blog-kill-current-buffer)
    (goto-char (point-min))
    (switch-to-buffer e-blog-choose-buffer)))

(defun e-blog-fetch-blog-feed (url)
  "Requests a feed from Blogger given a URL."
  (let (string)
    (save-excursion
      (set-buffer (get-buffer-create e-blog-tmp-buffer))
      (erase-buffer)
      (message "Requesting feed...")
      (call-process "curl" nil e-blog-tmp-buffer nil
		    "--stderr" "/dev/null"
		    "--header"
		    e-blog-auth
		    url)
      (setq string (buffer-substring (point-min) (point-max))))
    (message "Requesting feed... Done.")
    string))

(defun e-blog-expanded-to-collapsed ()
  "Changes the button at point to a `+' that will expand the list
that was collapsed."
  (save-excursion
    (delete-char 1)
    (insert-text-button "+"
			'action 'e-blog-expand-list
			'face 'e-blog-label)))

(defun e-blog-collapsed-to-expanded ()
  "Changes the button at point to a `-' that will collapse the
list that was expanded."
  (save-excursion
    (delete-char 1)
    (insert-text-button "-"
			'action 'e-blog-collapse-list
			'face 'e-blog-label)))

(defun e-blog-collapse-list (button)
  "Collapses a list of posts and saves it for expanding later."
  (save-excursion
    (let (beg button-pos collapsed)
      (setq button-pos (point))
      (forward-line 1)
      (setq beg (point))
      (search-forward "[X]\n\n")
      (setq collapsed (buffer-substring beg (point)))
      (delete-region beg (point))
      (goto-char button-pos)
      (delete-char 1)
      (insert-text-button "+"
			  'action 'e-blog-expand-list
			  'face 'e-blog-label
			  'collapsed collapsed))))

(defun e-blog-expand-list (button)
  "Restores a collapsed list of posts."
  (save-excursion
    (move-end-of-line 1)
    (insert "\n" (button-get button 'collapsed)))
  (e-blog-collapsed-to-expanded))

(defun e-blog-list-posts (button)
  "Asks Blogger for a list of posts for a single blog and creates
a list of buttons representing those posts."
  (let (blog-title user-feed blog-feed xml current-entry)
    (setq blog-title (button-get button 'title)
	  user-feed (button-get button 'feed))
    (setq xml (e-blog-fetch-blog-feed
	       (e-blog-get-post-url blog-title user-feed)))
    (setq blog-feed (e-blog-parse-xml xml))
    (save-excursion
      (move-end-of-line 1)
      (insert "\n")
      (dolist (title (e-blog-get-titles blog-feed))
	(setq current-entry
	      (e-blog-get-entry title blog-feed))
	(insert "\t    * ")
	(insert-text-button title
			    'action 'e-blog-edit-post
			    'face 'e-blog-url
			    'entry current-entry)
	(insert " [")
	(insert-text-button "X"
			    'action 'e-blog-confirm-delete
			    'face 'e-blog-title
			    'entry current-entry)
	(insert "]")
	(insert "\n"))))
  (e-blog-collapsed-to-expanded))

(defun e-blog-get-edit-url (entry)
  "Given an entry, returns an edit url."
  (let (edit-url)
    (setq edit-url
	  (nth 1 (assoc "edit" (e-blog-get-links entry))))
    edit-url))

(defun e-blog-insert-labels (labels)
  "Inserts labels, if any, to the edit buffer."
  (let (num-labels counter)
    (setq counter (length labels)
	  num-labels counter)
    (dolist (label labels)
      (setq counter (- counter 1))
      (insert label)
      (if (> counter 0)
	  (insert ", ")
	()))))

(defun e-blog-setup-post-buffer (button)
  "Sets up a buffer for writing a new post."
  (let (url)
    (setq url (button-get button 'url))
    (set-buffer (get-buffer-create e-blog-post-buffer))
    (e-blog-setup-common)
    (goto-char (point-min))
    (search-forward ": ")
    (insert url)
    (forward-char 1)
    (if e-blog-display-url
	()
      (narrow-to-region (point) (point-max)))
    (move-end-of-line 1)
    (local-set-key "\C-c\C-c" 'e-blog-extract-for-post)
    (e-blog-set-keybindings)
    (switch-to-buffer e-blog-post-buffer)))
    
(defun e-blog-post (prop-list)
  "Requests adding a post to a blog."
  (let (title content labels url rlist slist counter node-name)
    (kill-buffer (current-buffer))
    (setq title (nth 0 prop-list)
	  content (nth 1 prop-list)
	  labels (nth 2 prop-list)
	  url (nth 3 prop-list)
	  node-name "<category scheme=\"http://www.blogger.com/atom/ns#\" term=\"")
    (set-buffer (get-buffer-create e-blog-tmp-buffer))
    (erase-buffer)
    (insert e-blog-post-xml)
    (goto-char (point-min))
    (setq rlist '("<!-- @@@Title@@@ -->"
		  "<!-- @@@Text@@@ -->"
		  "<!-- @@@User Name@@@ -->"
		  "<!-- @@@email@@@ -->"))
    (setq slist (list title
		      content
		      user-full-name
		      e-blog-user))
    (setq counter 0)
    (dolist (repl rlist)
      (search-forward repl nil t)
      (replace-match (nth counter slist))
      (setq counter (+ counter 1)))
    (goto-char (point-min))
    (search-forward "</title>")
    (insert "\n")
    (if (equal (nth 0 labels) "")
	()
      (dolist (label labels)
	(insert "  " node-name label "\"/>\n")))
    (delete-blank-lines)
    (set-visited-file-name "/tmp/e-blog-tmp")
    (setq buffer-file-coding-system 'utf-8)
    (save-buffer)
    (message "Sending Post...")
    (call-process "curl" nil e-blog-buffer nil
		  "-v" "--header"
		  e-blog-auth
		  "--header" "Content-Type: application/atom+xml"
		  "-d" "@/tmp/e-blog-tmp"
		  url)
    (e-blog-cleanup)
    (message "Sending Post... Done.")))
      
(defun e-blog-setup-edit-buffer (title labels content edit-url)
  "Sets up a buffer for editing a post."
  (let (beg-narrow beg-content)
  (set-buffer (get-buffer-create
	       (concat e-blog-edit-buffer title "*")))
  (e-blog-setup-common)
  (goto-char (point-min))
  (move-end-of-line 1)
  (insert edit-url)
  (forward-line 1)
  (setq beg-narrow (point))
  (move-end-of-line 1)
  (insert title)
  (forward-line 1)
  (move-end-of-line 1)
  (e-blog-insert-labels labels)
  (goto-char (point-max))
  (insert content)
  (e-blog-do-markdowns)
  (if e-blog-display-url
      ()
    (narrow-to-region beg-narrow (point-max)))
  (local-set-key "\C-c\C-c" 'e-blog-extract-for-edit)
  (e-blog-set-keybindings)
  (switch-to-buffer (concat e-blog-edit-buffer title "*"))))

(defun e-blog-extract-common ()
  "Extracts the title, labels, content, and post/edit url from
the post/edit buffer."
  (let (beg title content labels url post-info)
    (widen)
    (goto-char (point-min))
    (search-forward ": ")
    (setq beg (point))
    (forward-line)
    (setq url (buffer-substring beg (point)))
    (search-forward ": ")
    (setq beg (point))
    (forward-line)
    (setq title (buffer-substring beg (point)))
    (search-forward ": ")
    (setq labels (e-blog-extract-labels))
    (forward-line 2)
    (setq beg (point))
    (e-blog-do-markups)
    (setq content (buffer-substring beg (point-max)))
    (setq post-info (list title content labels url))
    post-info))

(defun e-blog-do-markups ()
  "Does very simple marking up to make content suitable for XML
encapsulation."
  (interactive)
  (let (replacements beg-text)
    (setq replacements
	  '(("\n\n" "</p><p>")
	    ("\n" " ")
	    ("</p><p>" "</p>\n<p>")))
    (setq beg-text (point))
    (insert-string "<p>")
    (dolist (list replacements)
      (while (search-forward (car list) nil t)
	(replace-match (nth 1 list)))
      (goto-char beg-text))
    (goto-char (point-max))
    (insert-string "</p>")))

(defun e-blog-post-edit (prop-list)
  "Requests editing of an existing entry on a blog."
  (let (title content labels url entry)
    (kill-buffer (current-buffer))
    (setq title (nth 0 prop-list)
	  content (nth 1 prop-list)
	  labels (nth 2 prop-list)
	  url (nth 3 prop-list))
    (setq entry
	 (xml-node-name (e-blog-parse-xml
	   (e-blog-fetch-blog-feed url))))
    (e-blog-change-title entry title)
    (e-blog-change-content entry content)
    (e-blog-elisp-to-xml entry)
    ;; The rest of this function is performed in `e-blog-tmp-buffer'
    ;; since the `e-blog-elisp-to-xml' did a `set-buffer'.
    (e-blog-change-labels labels)
    (set-visited-file-name "/tmp/e-blog-tmp")
    (setq buffer-file-coding-system 'utf-8)
    (save-buffer)
    (message "Sending request for edit...")
    (call-process "curl" nil e-blog-buffer nil
		  "--header"
		  e-blog-auth
		  "--header" "Content-Type: application/atom+xml"
		  "-X" "PUT" "-d" "@/tmp/e-blog-tmp"
		  url)
    (e-blog-cleanup)
    (message "Sending request for edit... Done." )))

(defun e-blog-confirm-delete (button)
  "Asks for confirmation before deleting a post."
  (if (y-or-n-p "Are you sure you want to delete this post? ")
      (e-blog-delete-post button)
    (message "Post not deleted.")))

(defun e-blog-delete-post (button)
  "Requests deletion of a post."
  (let (entry url)
    (setq entry (button-get button 'entry)
	  url (e-blog-get-edit-url entry))
    (message "Sending request to delete post...")
    (call-process "curl" nil e-blog-buffer nil
		  "--header" 
		  e-blog-auth
		  "-X" "DELETE"
		  url)
    (move-beginning-of-line nil)
    (setq beg (point))
    (move-end-of-line nil)
    (delete-region beg (+ (point) 1))
    (message "Sending request to delete post... Done.")))

(defun e-blog-cleanup ()
  "Kills some unneeded buffers."
  (delete-file "/tmp/e-blog-tmp")
  (kill-buffer e-blog-choose-buffer)
  (kill-buffer "e-blog-tmp"))

(defun e-blog-elisp-to-xml (elisp)
  "Converts XML represented in elisp back to XML and does some
encapsulation of the content to ensure suitability for Blogger
requests."
  (set-buffer (get-buffer-create e-blog-tmp-buffer))
  (erase-buffer)
  (xml-debug-print-internal elisp " ")
  (goto-char (point-min))
  (search-forward "<content type=\"html\">")
  (replace-match "<content type='xhtml'>")
  (insert "<div xmlns=\"http://www.w3.org/1999/xhtml\">")
  (search-forward "</content>")
  (search-backward "</content>")
  (insert "</div>")
  (goto-char (point-min))
  (while (search-forward "&" nil t)
    (replace-match "&amp;"))
  (goto-char (point-min))
  (while (search-forward "\"" nil t)
    (replace-match "'")))

(defun e-blog-extract-for-edit ()
  "Calls `e-blog-extract-common' for editing a post."
  (interactive)
  (e-blog-post-edit (e-blog-extract-common)))

(defun e-blog-extract-for-post ()
  "Calls `e-blog-extract-common' for posting a post."
  (interactive)
  (e-blog-post (e-blog-extract-common)))

(defun e-blog-extract-labels ()
  "Extracts comma separated list of labels from a post/edit
buffer."
  (let (label labels beg eol)
    (setq beg (point)
	  labels ())
    (move-end-of-line nil)
    (setq eol (point))
    (goto-char beg)
    (while (search-forward "," eol t)
      (setq label (buffer-substring beg (- (point) 1)))
      (add-to-list 'labels label)
      (setq beg (point)))
    (add-to-list 'labels (buffer-substring beg eol))
    labels))

(defun e-blog-do-markdowns ()
  "Marks down retrieved content from html to text."
  (let (beg-text beg end replacements)
    (move-beginning-of-line nil)
    (setq beg-text (point))
    (setq replacements
	  '(("&lt;" "<")
	    ("&gt;" ">")
	    ("<p>" "")
	    ("</p>" "\n\n")
	    ("<div xmlns='http://www.w3.org/1999/xhtml'>" "")
	    ("</div>" "")
	    ("<br />" "")))
    (dolist (list replacements)
      (goto-char beg-text)
      (while (search-forward (car list) nil t)
	(replace-match (nth 1 list))))
    (goto-char beg-text)
    (re-search-forward " *")
    (delete-region beg-text (point))
    (goto-char (point-max))
    (forward-line -2)
    (move-end-of-line nil)
    (delete-region (point) (point-max))))

(defun e-blog-edit-post (button)
  "Calls necessary functionf for preparing an edit for a Blogger
request."
  (let (entry)
    (setq entry (button-get button 'entry))
    (e-blog-setup-edit-buffer
     (button-label button)
     (e-blog-get-labels entry)
     (e-blog-get-content entry)
     (e-blog-get-edit-url entry))))

(defun e-blog-setup-common ()
  "Does the common buffer setup for posting/editing."
  (let (u-string t-string l-string p-string all faces cur-face counter)
    (setq u-string "Url: \n"
	  t-string "Title: \n"
	  l-string "Labels: \n"
	  p-string "-------- Post Follows This Line -------- \n"
	  all (list u-string t-string l-string p-string)
	  faces '(e-blog-url e-blog-title e-blog-label e-blog-post)
	  counter 0)
    (insert u-string t-string l-string p-string)
    (goto-char (point-min))
    (dolist (string all)
      (add-text-properties (point) (- (+ (point) (length string)) 2)
			   (list 'read-only "Please type your entries after the colored text."
				 'face (nth counter faces)))
      (forward-line)
      (setq counter (+ 1 counter)))
      (auto-fill-mode 1)))

(defun e-blog-new-post ()
  "Initializes e-blog."
  (interactive)
  (if e-blog-auth
      (e-blog-choose)
    (progn (e-blog-do-auth) (e-blog-choose))))

(defun e-blog-do-auth ()
  "Calls the functions necessary for communicating with Gdata."
  (e-blog-fetch-auth)
  (if (e-blog-check-authinfo)
      (e-blog-choose)))

(defun e-blog-choose ()
  "Called to setup a choose buffer."
  (e-blog-setup-choose-buffer (e-blog-parse-xml (e-blog-fetch-bloglist))))

(defun e-blog-parse-xml (string)
  "Parses XML to be represented in elisp."
  (let (parsed)
    (save-excursion
      (set-buffer (get-buffer-create e-blog-tmp-buffer))
      (erase-buffer)
      (insert string)
      (setq parsed (xml-parse-region (point-min) (point-max)))
      parsed)))

(defun e-blog-get-titles (feed)
  "Given a elisp representation of a FEED, returns the titles
from that FEED."
  (let (titles)
    (setq titles ())
    (dolist (entry (e-blog-get-entries feed))
      (add-to-list 'titles
		   (e-blog-get-title entry)))
    titles))

(defun e-blog-get-entries (feed)
  "Given an elisp representation of a FEED, returns the entries
from that FEED."
  (let (entries)
    (setq entries (xml-get-children (xml-node-name feed) 'entry))
    entries))

(defun e-blog-get-title (entry)
  "Given an  elisp representation of an ENTRY,  returns the title
of that ENTRY."
  (let (title-tag title)
    (setq title-tag
	  (xml-get-children entry 'title))
    (setq title
	  (nth 0
	       (xml-node-children
		(xml-node-name title-tag))))
    title))

(defun e-blog-get-post-url (title feed)
  "Given an elisp representation of a FEED and a TITLE, returns
the post url for that TITLE."
  (let (post-url links)
    (dolist (entry (e-blog-get-entries feed))
      (if (equal (e-blog-get-title entry) title)
	(setq post-url
	      (nth 1 (assoc e-blog-post-url-rel
			    (e-blog-get-links entry))))))
    post-url))

(defun e-blog-get-links (entry)
  "Given an elisp representation of an ENTRY, returns a list of
the links associated with that ENTRY."
  (let (links type-link)
    (dolist (link (xml-get-children entry 'link))
      (setq type-link
	    (list (xml-get-attribute link 'rel)
		  (xml-get-attribute link 'href)))
      (add-to-list 'links
		   type-link))
    links))

(defun e-blog-get-entry (title feed)
  "Given an elisp representation of a FEED and a TITLE, returns
an elisp representation of the entry for that TITLE."
  (let (matching-entry)
  (dolist (entry (e-blog-get-entries feed))
    (if (equal (e-blog-get-title entry) title)
	(setq matching-entry entry)))
  matching-entry))

(defun e-blog-get-labels (entry)
  "Given an elisp representation of an ENTRY, returns a list of
labels for that ENTRY."
  (let (post-labels)
    (setq post-labels ())
    (dolist (label (xml-get-children entry 'category))
      (add-to-list 'post-labels
		   (xml-get-attribute label 'term)))
    post-labels))

(defun e-blog-get-content (entry)
  "Given an elisp representation of an ENTRY, returns the content
for that ENTRY."
  (let (content)
    (setq content
	  (nth 0
	       (xml-node-children
		(xml-node-name
		 (xml-get-children entry 'content)))))
    content))

(defun e-blog-change-title (entry title)
  "Given an elisp representation of an entry and a title, will
modify the title to given title."
  (setcar (xml-node-children
	   (xml-node-name
	    (xml-get-children entry 'title)))
	  title))

(defun e-blog-change-content (entry content)
  "Given an elisp representation of an ENTRY and CONTENT, will
modify the content in ENTRY to given CONTENT."
  (setcar (xml-node-children
	   (xml-node-name
	    (xml-get-children entry 'content)))
	  content))

(defun e-blog-change-labels (labels)
  "Given a list of LABELS, modifies XML to represent those LABELS
and removes any existing labels, if applicable"
  (let (beg node-name)
    (setq node-name "<category scheme='http://www.blogger.com/atom/ns#' term='")
    (set-buffer e-blog-tmp-buffer)
    (goto-char (point-min))
    (while (search-forward node-name nil t)
      (move-beginning-of-line 1)
      (setq beg (point))
      (move-end-of-line 1)
    (delete-region beg (point))
    (delete-blank-lines))
    (goto-char (point-min))
    (search-forward "</updated>")
    (insert "\n")
    (if (equal (nth 0 labels) "")
	()
      (dolist (label labels)
	(insert "  " node-name label "'/>\n")))
    (delete-blank-lines)))

(defun e-blog-link-region (mk pt addr)
  (interactive "r\nshttp://")
  (let (link-text)
    (setq link-text (buffer-substring mk pt))
    (delete-region mk pt)
    (goto-char mk)
    (insert "<a href=\"http://" addr "\">" link-text "</a>")))

(defun e-blog-tt-region (mk pt)
  (interactive "r")
  (let (tt-text)
    (setq tt-text (buffer-substring mk pt))
    (delete-region mk pt)
    (goto-char mk)
    (insert "<tt>" tt-text "</tt>")))

(defun e-blog-emphasize-region (mk pt)
  (interactive "r")
  (let (em-text)
    (setq em-text (buffer-substring mk pt))
    (delete-region mk pt)
    (goto-char mk)
    (insert "<em>" em-text "</em>")))

(defun e-blog-strong-region (mk pt)
  (interactive "r")
  (let (strong-text)
    (setq strong-text (buffer-substring mk pt))
    (delete-region mk pt)
    (goto-char mk)
    (insert "<strong>" strong-text "</strong>")))

(defun e-blog-set-keybindings ()
  (let (bindlist defunlist counter)
    (setq bindlist
	  (list e-blog-link-region-key
		e-blog-tt-region-key
		e-blog-emphasize-region-key
		e-blog-strong-region-key)
	  defunlist
	  '(e-blog-link-region
	    e-blog-tt-region
	    e-blog-emphasize-region
	    e-blog-strong-region)
	  counter 0)
    (dolist (binding bindlist)
      (local-set-key binding (nth counter defunlist))
      (setq counter (+ counter 1)))))
      
(setq e-blog-post-xml 
"<entry xmlns='http://www.w3.org/2005/Atom'>
  <title type='text'><!-- @@@Title@@@ --></title>
  <content type='xhtml'>
    <div xmlns=\"http://www.w3.org/1999/xhtml\">
      <!-- @@@Text@@@ -->
    </div>
  </content>
  <author>
    <name><!-- @@@User Name@@@ --></name>
    <email><!-- @@@email@@@ --></email>
  </author>
</entry>")
