;; Firefox quicksearch for emacs
;; Version 0.1
;;
;; This code reads through your firefox customization (or some of it, at least)
;; to discover which search plugins you have installed. It then understands
;; enough of them to be able to do its own searches with this information.
;;
;; This uses browse-url, so configure that to whatever you like best: w3m's
;; always good!
;;
;; To install, make sure you have xml, thingatpt and browse-url and then check
;; that the starred variables below "Customizable stuff?" are set to the correct
;; values (if anyone likes this, I'll set them to be defcustom).
;;
;; Hopefully, FFS-DO-SEARCH should now work for you: I have the following
;; keyboard shortcut set up:
;;
;;    (global-set-key [f6]  'ffs-do-search)
;;
;; But obviously others will work too.
;;
;;
;; If you use this, please let me know - I'd be interested to hear peoples'
;; experiences: on the plus side, I have been happily using this for some
;; months, so it should all work reasonably stably.
;;
;;
;; Rupert Swarbrick (17/07/08)
;; (rot13 "efjneoevpx@tznvy.pbz") => My email address.
;;
;; for those without emacs handy, that's rswarbrick at google's mail provider
;;
;;
;; License: GPLv3


(require 'cl)
(require 'xml)
(require 'thingatpt)
(require 'browse-url)

;; Customizable stuff?
(defvar *ff-root-pref-dir*
  "~/.mozilla/firefox/"
  "The root of the firefox profiles directory tree")
(defvar *ff-install-dir*
  "/usr/share/iceweasel/"
  "Root of firefox install dir")
(defvar *ff-preferences-dir*
  (let ((list (directory-files *ff-root-pref-dir* t "default")))
    (when (and list (consp list))
      (car list)))
  "The directory for the profile we're considering")

(defvar ffs-searches
  nil
  "The list of search methods. Populated @ bottom of file")

(defvar ffs-default-engine
  nil
  "The default engine. Set @ bottom of file")

;; Error handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'ff-fp-error 'error-conditions '(error ffs-errors ff-fp-error))
(put 'ff-fp-error 'error-message "File parse error")

;; XML and general utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-xml-tagname (symbol ns-sym)
  "Create a symbol that is SYMBOL with the namespace referred to
by NS-SYM prepended. eg
  (build-xml-tagname 'hello 'xmlns:message) => 'message:hello
and
  (build-xml-tagname 'hello 'amessage) => 'amessage:hello
and (the trivial case)
  (build-xml-tagname 'hello nil) => 'hello"
  (let*
      ((ns-str (if ns-sym (symbol-name ns-sym) ""))
       (ns
        (if (and (> (length ns-str) 6)
                 (string= (substring ns-str 0 6) "xmlns:"))
            (substring ns-str 6)
          ns-str)))
    (make-symbol
     (concat ns
             (if (string= ns "") "" ":")
             (symbol-name symbol)))))

(defun descend-tree-collecting (root
                                pred txform
                                child-func
                                descend-pred
                                descent-func)
  "Utility function to descend tree structures. ROOT is the
starting node. We call CHILD-FUNC on ROOT to get a list of
children. For each child, if PRED is true, we collect TXFORM of
the child. If DESCEND-PRED is true, then we call DESCENT-FUNC on
the child to recurse with the result as the new root."

  (let (swag)
    (loop
                                        ; Iterate over the abstract children collection.
     for sprog in (funcall child-func root)
                                        ; If they're suitable, push a transformed version onto our list.
     when (funcall pred sprog) do
     (push (funcall txform sprog) swag)
                                        ; If we should descend, do so.
     when (funcall descend-pred sprog) do
     (nconc (descend-tree-collecting
             (funcall descent-func sprog)
             pred txform child-func descend-pred descent-func)
            swag))
    swag))

(defun xml-get-descendants (node name &optional attrs)
  "Return the descendants of NODE whose tag is NAME.
NAME should match the value returned by `xml-node-name'. If
ATTRS, then it is an alist of attribute name/value pairs
required."
  (descend-tree-collecting
   node
   (lambda (x)
     (and (consp x)
          (not (stringp (car x)))
          (string= (symbol-name (xml-node-name x))
                   (symbol-name name))
          (or (not attrs)
              (every
               (lambda (attr)
                 (string= (xml-get-attribute x (car attr))
                          (cdr attr)))
               attrs))))
   #'identity #'xml-node-children #'consp #'identity))

(defun find-node-or-error (root name errmsg)
  "Find the first node called NAME under ROOT, throwing an error
if there is none."
  (let ((result (xml-get-descendants root name)))
    (unless result
      (signal 'ff-fp-error errmsg))
    (car result)))

(defmacro check-xml-attribute (tag attr expected err-data-form)
  `(unless (string= (xml-get-attribute ,tag ,attr) ,expected)
     (signal 'ff-fp-error ,err-data-form)))

(defun string-search-and-replace (search replace string)
  "Replace all instances of SEARCH with REPLACE in STRING."
  (replace-regexp-in-string (regexp-quote search) replace string))

(defun minibuffer-read-with-default
  (prompt default &optional history-list nil-sensible)

  "Read uncompleted input from the minibuffer using PROMPT and
giving a default value of DEFAULT if no text entered. Unless
NIL-SENSIBLE, passing '' is the same as passing nil for DEFAULT."

  (unless history-list
    (setf history-list 'minibuffer-history))
  (when (and (string= "" default) (not nil-sensible))
    (setf default nil))

  (let ((result (read-from-minibuffer
                 (concat prompt
                         (when default
                           (format "[Default: \"%s\"] " default)))
                 nil nil nil
                 history-list)))
    (when (string= result "") (setf result nil))
    (unless result
      (when (and default
                 (not (string= "" default)))
        (add-to-history history-list default)))

    (if result result default)))

;; Generic rebuilding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ff-search-plugin-files ()
  (append
   (directory-files
    (concat (file-name-as-directory *ff-preferences-dir*)
            "searchplugins")
    t "\\.xml")
   (directory-files
    (concat (file-name-as-directory *ff-install-dir*)
            "searchplugins")
    t "\\.xml")))

(defun ffs-read-all-files ()
  (let ((tmp-result))
    (loop
     for xml-file in (ff-search-plugin-files)

     do (setf tmp-result nil)

     do
     (condition-case err
         (setf tmp-result (ffs-read-file xml-file))
       (ffs-errors
        (message "%s: %s" (error-message-string err) (cdr err))))

     when tmp-result collect tmp-result)))

;; Dispatch on type of xml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ffs-read-file (fname)
  (let* ((root (car (xml-parse-file fname)))
         (os-namespace (rassoc "http://a9.com/-/spec/opensearch/1.1/"
                               (xml-node-attributes root)))
         (url-tag)
         (name-tag))
    (cond
     ((and (equal (xml-node-name root) 'SearchPlugin)
           (string= (xml-get-attribute root 'xmlns)
                    "http://www.mozilla.org/2006/browser/search/")
           (not os-namespace))
      (ffs-read-mozilla-search fname root))

     (os-namespace
      (ffs-read-opensearch root os-namespace fname))

     (t
      (message "Ffs can't understand the XML in %s." fname)
      nil))))

;; Opensearch parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ffs-read-opensearch (xml namespace fname)
  (let ( ;; Now we want os:Url
        (url-tag
         (find-node-or-error
          xml
          (build-xml-tagname 'Url (car namespace))
          (format "No opensearch url in file %s" fname)))
        ;; and os:ShortName
        (name-tag
         (find-node-or-error
          xml
          (build-xml-tagname 'ShortName (car namespace))
          (format "No opensearch ShortName in file %s" fname))))

    (check-xml-attribute
     url-tag 'method "GET"
     (format "In file %s, the os:Url uses %s but we only do GET"
             fname (xml-get-attribute url-tag 'method)))

    (check-xml-attribute
     url-tag 'type "text/html"
     (format "File %s: the os:Url takes %s but we only do text/html"
             fname (xml-get-attribute url-tag 'type)))

    (cons
     (car (xml-node-children name-tag))
     (xml-get-attribute url-tag 'template))))

;; Mozilla search plugin parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ffs-read-mozilla-search (fname xml)
  (let ((url-tags (xml-get-descendants xml 'Url '((type . "text/html")
                                                  (method . "GET"))))
        (url))
    (cond
     ((null url-tags)
      (message "No matching Url tag in %s" fname)
      nil)
     (t
      (setf url (concat (xml-get-attribute (car url-tags) 'template) "?"))
      ;; Now we have to go through each of the params and add them to the url
      (loop
       for param in (xml-get-descendants (car url-tags) 'Param)
       do
       (setf url (concat url
                         (xml-get-attribute param 'name)
                         "="
                         (xml-get-attribute param 'value)
                         "&")))

      (cons
       (car (xml-node-children
             (car (xml-get-descendants xml 'ShortName))))
       (substring url 0 (1- (length url))))))))

;; The default ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ffs-find-default-search ()
  ;; This is an ff preference, so look in prefs.js (eugh)
  (with-temp-buffer
    (insert-file-contents
     (concat (file-name-as-directory *ff-preferences-dir*) "prefs.js"))
    (when (search-forward
           "user_pref(\"browser.search.selectedEngine\""
           (point-max) t)
      (search-forward "\"" (point-max) t)
      (set-mark (point))
      (search-forward "\"" (point-max) t)
      (let ((name (buffer-substring (mark) (1- (point)))))
        (when (assoc name ffs-searches) name)))))

;; Actually do searches ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ffs-do-search ()
  (interactive)

  (unless ffs-searches
    (setf ffs-searches (ffs-read-all-files))
    (unless ffs-default-engine
      (setf ffs-default-engine
            (or (ffs-find-default-search)
                (caar ffs-searches)))))

  (let ((str (minibuffer-read-with-default
              "Search for: " (thing-at-point 'symbol))))

    (unless (or (null str) (string= "" str))

      (let ((eng (completing-read
                  (format "Using (default: %s) " ffs-default-engine)
                  (mapcar 'car ffs-searches))))

        (when (string= "" eng) (setq eng ffs-default-engine))

        (setf ffs-default-engine eng)

        (browse-url
         (string-search-and-replace
          "{searchTerms}" str (cdr (assoc eng ffs-searches))))))))
