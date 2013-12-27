;;;Auto generated

;;;### (autoloads (gcal-sign-in gcal-sign-out gcal-emacs-calendar-setup
;;;;;;  gcal-show-event gcal-view gcal-calendar-agenda-days gcal-delete-event
;;;;;;  gcal-quickadd-event gcal-add-event gcal-user-email gcal-default-user-email)
;;;;;;  "gcal" "gcal.el" (18401 35641))
;;; Generated autoloads from gcal.el

(defvar gcal-default-user-email nil "\
Default user id for Calendar.")

(custom-autoload 'gcal-default-user-email "gcal" t)

(defvar gcal-user-email nil "\
Mail address that identifies calendar user.")

(custom-autoload 'gcal-user-email "gcal" t)

(autoload 'gcal-add-event "gcal" "\
Add a calendar event.

\(fn)" t nil)

(autoload 'gcal-quickadd-event "gcal" "\
Add a calendar event.
Specify the event in plain English.

\(fn EVENT-DESC)" t nil)

(autoload 'gcal-delete-event "gcal" "\
Delete a calendar event.

\(fn EVENT-URI)" t nil)

(defvar gcal-calendar-agenda-days 5 "\
Number of days for which we show an agenda by default.")

(custom-autoload 'gcal-calendar-agenda-days "gcal" t)

(autoload 'gcal-view "gcal" "\
Retrieve and display resource after authenticating.

\(fn RESOURCE)" t nil)

(autoload 'gcal-show-event "gcal" "\
Show event at URL.

\(fn URL)" t nil)

(define-prefix-command 'gcal-calendar-prefix-map)

(autoload 'gcal-emacs-calendar-setup "gcal" "\
Setup GCal keybindings in Emacs calendar.

\(fn)" nil nil)

(autoload 'gcal-sign-out "gcal" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'gcal-sign-in "gcal" "\
Sign in, useful when changing to a different user profile.

\(fn)" t nil)

;;;***

;;;### (autoloads (gphoto-edit-entry gphoto-sign-in gphoto-sign-out
;;;;;;  gphoto-comment-or-tag gphoto-directory-add-photos gphoto-photo-add
;;;;;;  gphoto-album-create gphoto-user-tagsearch gphoto-user-search
;;;;;;  gphoto-recent gphoto-community-search gphoto-download gphoto-view
;;;;;;  gphoto-tags gphoto-albums gphoto-feeds) "gphoto" "gphoto.el"
;;;;;;  (18085 7409))
;;; Generated autoloads from gphoto.el

(autoload 'gphoto-feeds "gphoto" "\
Retrieve and display feed of albums or tags after authenticating.

\(fn KIND)" t nil)

(autoload 'gphoto-albums "gphoto" "\
Display feed of albums.

\(fn)" t nil)

(autoload 'gphoto-tags "gphoto" "\
View feed of tags.

\(fn)" t nil)

(autoload 'gphoto-view "gphoto" "\
Retrieve and display resource after authenticating.

\(fn RESOURCE)" t nil)

(autoload 'gphoto-download "gphoto" "\
Download resource after authenticating.

\(fn RESOURCE)" t nil)

(autoload 'gphoto-community-search "gphoto" "\
Search all public photos.

\(fn QUERY)" t nil)

(autoload 'gphoto-recent "gphoto" "\
Retrieve feed of recently uploaded photos or comments.

\(fn USER KIND)" t nil)

(autoload 'gphoto-user-search "gphoto" "\
Retrieve feed o recently uploaded comments for  specified user.

\(fn USER QUERY)" t nil)

(autoload 'gphoto-user-tagsearch "gphoto" "\
Retrieve feed o matches comments for  specified user.

\(fn USER TAG)" t nil)

(autoload 'gphoto-album-create "gphoto" "\
Create a new GPhoto album.

\(fn)" t nil)

(autoload 'gphoto-photo-add "gphoto" "\
Add a photo to an existing album.

\(fn ALBUM-NAME PHOTO)" t nil)

(autoload 'gphoto-directory-add-photos "gphoto" "\
Add all jpeg files in a directory to specified album.

\(fn DIRECTORY ALBUM-NAME)" t nil)

(autoload 'gphoto-comment-or-tag "gphoto" "\
Add comments or tags  to an existing photo.

\(fn TYPE RESOURCE)" t nil)

(autoload 'gphoto-sign-out "gphoto" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'gphoto-sign-in "gphoto" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'gphoto-edit-entry "gphoto" "\
Retrieve metadata for entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry.

\(fn URL)" t nil)

;;;***

;;;### (autoloads (greader-sign-in greader-sign-out greader-search
;;;;;;  greader-find-feeds greader-star greader-add-label greader-untag-feed
;;;;;;  greader-tag-feed greader-title-feed greader-unsubscribe-feed
;;;;;;  greader-subscribe-feed greader-opml greader-feed-list greader-preferences
;;;;;;  greader-reading-list) "greader" "greader.el" (18380 12489))
;;; Generated autoloads from greader.el

(autoload 'greader-reading-list "greader" "\
Ensure our cookies are live, and get the reading list.
Optional interactive prefix `state' prompts for state to retrieve

e.g., starred.

\(fn &optional STATE)" t nil)

(autoload 'greader-preferences "greader" "\
Ensure our cookies are live, and get all preferences for this
user.

\(fn)" t nil)

(autoload 'greader-feed-list "greader" "\
Retrieve list of subscribed feeds.
Feeds are sorted by timestamp of newly arrived articles.
Optional interactive prefix arg `sort' turns on sorting.

\(fn &optional SORT)" t nil)

(autoload 'greader-opml "greader" "\
Retrieve OPML representation of our subscription list.

\(fn)" t nil)

(autoload 'greader-subscribe-feed "greader" "\
Subscribe to specified feed.

\(fn FEED-URL)" t nil)

(autoload 'greader-unsubscribe-feed "greader" "\
UnSubscribe from specified feed.

\(fn FEED-URL)" t nil)

(autoload 'greader-title-feed "greader" "\
Title  specified feed.

\(fn FEED-URL)" t nil)

(autoload 'greader-tag-feed "greader" "\
Tag  specified feed.

\(fn FEED-URL)" t nil)

(autoload 'greader-untag-feed "greader" "\
Remove Tag from specified feed.

\(fn FEED-URL)" t nil)

(autoload 'greader-add-label "greader" "\
Add label to this item.

\(fn ITEM-URL LABEL)" t nil)

(autoload 'greader-star "greader" "\
Star this item.

\(fn ITEM-URL)" t nil)

(autoload 'greader-find-feeds "greader" "\
Find feeds matching query.

\(fn QUERY)" t nil)

(autoload 'greader-search "greader" "\
GReader search.

\(fn QUERY)" t nil)

(autoload 'greader-sign-out "greader" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'greader-sign-in "greader" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***

;;;### (autoloads (gsheet-sign-in gsheet-sign-out gsheet-sheets gsheet-fetch)
;;;;;;  "gsheet" "gsheet.el" (18055 59332))
;;; Generated autoloads from gsheet.el

(autoload 'gsheet-fetch "gsheet" "\
Fetch specified sheet.

\(fn SHEET-URL)" t nil)

(autoload 'gsheet-sheets "gsheet" "\
Retrieve and display feed of feeds after authenticating.

\(fn)" t nil)

(autoload 'gsheet-sign-out "gsheet" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'gsheet-sign-in "gsheet" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***

;;;### (autoloads (gskeleton-sign-in gskeleton-sign-out) "gskeleton"
;;;;;;  "gskeleton.el" (18055 59332))
;;; Generated autoloads from gskeleton.el

(autoload 'gskeleton-sign-out "gskeleton" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'gskeleton-sign-in "gskeleton" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***

;;;### (autoloads (gtube-video-featured gtube-video-by-user gtube-video-popular
;;;;;;  gtube-video-playlist gtube-video-by-category-and-tag gtube-video-by-tag
;;;;;;  gtube-video-details gtube-user-friends gtube-user-favorites
;;;;;;  gtube-user-profile) "gtube" "gtube.el" (18268 39858))
;;; Generated autoloads from gtube.el

(autoload 'gtube-user-profile "gtube" "\
Retrieve user profile.

\(fn &optional USER)" t nil)

(autoload 'gtube-user-favorites "gtube" "\
Retrieve user favorites.

\(fn &optional USER)" t nil)

(autoload 'gtube-user-friends "gtube" "\
Retrieve user profile.

\(fn &optional USER)" t nil)

(autoload 'gtube-video-details "gtube" "\
Display details of specified video.

\(fn VIDEO-ID)" t nil)

(autoload 'gtube-video-by-tag "gtube" "\
Retrieve content having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn TAG &optional PAGE COUNT)" t nil)

(autoload 'gtube-video-by-category-and-tag "gtube" "\
Retrieve content from specified category having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn CATEGORY TAG &optional PAGE COUNT)" t nil)

(autoload 'gtube-video-playlist "gtube" "\
Retrieve content in specified playlist.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn PLAYLIST-ID &optional PAGE COUNT)" t nil)

(autoload 'gtube-video-popular "gtube" "\
Retrieve popular content for specified time-range.
  Time-range is one of day, week, month, or all.

\(fn TIME-RANGE)" t nil)

(autoload 'gtube-video-by-user "gtube" "\
Retrieve content from specified user.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn USER &optional PAGE COUNT)" t nil)

(autoload 'gtube-video-featured "gtube" "\
Retrieved featured video list.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("g-app.el" "g-auth.el" "g-autogen.el"
;;;;;;  "g-load-path.el" "g-utils.el" "g.el" "gnotebook.el" "indent-files.el"
;;;;;;  "json.el") (18533 39805 283567))

;;;***

;;;### (autoloads (gblogger-sign-in gblogger-sign-out gblogger-add-label
;;;;;;  gblogger-delete-entry gblogger-new-entry gblogger-edit-entry
;;;;;;  gblogger-atom-display gblogger-blog) "gblogger" "gblogger.el"
;;;;;;  (18278 62171))
;;; Generated autoloads from gblogger.el

(autoload 'gblogger-blog "gblogger" "\
Retrieve and display feed of feeds after authenticating.

\(fn)" t nil)

(autoload 'gblogger-atom-display "gblogger" "\
Retrieve and display specified feed after authenticating.

\(fn FEED-URL)" t nil)

(autoload 'gblogger-edit-entry "gblogger" "\
Retrieve entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry.

\(fn URL)" t nil)

(autoload 'gblogger-new-entry "gblogger" "\
Create a new Blog post.

\(fn URL)" t nil)

(autoload 'gblogger-delete-entry "gblogger" "\
Delete item at specified edit URL.

\(fn EDIT-URL)" t nil)

(autoload 'gblogger-add-label "gblogger" "\
Adds labels to gblogger entry being editted.

\(fn LABEL)" t nil)

(autoload 'gblogger-sign-out "gblogger" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload 'gblogger-sign-in "gblogger" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***
