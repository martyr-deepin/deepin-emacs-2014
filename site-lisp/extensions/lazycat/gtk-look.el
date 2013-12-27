;;; gtk-look.el --- lookup Gtk and Gnome documentation.

;; Copyright 2004, 2006, 2007, 2008 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 10
;; Keywords: tools, c
;; URL: http://www.geocities.com/user42_kevin/gtk-look/index.html
;; EmacsWiki: GtkLook

;; gtk-look.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; gtk-look.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses>.


;;; Commentary:

;; M-x gtk-lookup-symbol displays HTML documentation for Gtk and Gnome
;; functions and variables, similar to what M-x info-lookup-symbol does for
;; info files.  The documentation is expected to be HTML files with devhelp
;; indexes, like the Debian packages libgtk2.0-doc etc.  See the
;; `gtk-lookup-symbol' docstring below for more.

;;; Install:

;; Put gtk-look.el somewhere in your `load-path', and in your .emacs put
;;
;;     (autoload 'gtk-lookup-symbol "gtk-look" nil t)
;;
;; This makes M-x gtk-lookup-symbol available, but you'll probably want to
;; bind it to a key.  C-h C-j is one possibility, being close to C-h C-i for
;; `info-lookup-symbol'.  For instance to do that globally,
;;
;;     (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol)

;;; Emacsen:

;; Designed for Emacs 21 and 22.
;;
;; Works in XEmacs 21 if you copy `file-expand-wildcards' from the Emacs
;; sources and if you byte-compile to avoid slowness in the compatibility
;; code below.

;;; History:

;; Version 1 - the first version
;; Version 2 - correction to usual .devhelp file locations
;; Version 3 - recognise devhelp2 format
;; Version 4 - home page link, more parens on funcs in index,
;;             fix lookup done from within an existing browser buffer
;; Version 5 - make browse other window work in xemacs
;; Version 6 - use with-compression-mode and display-warning, when available
;; Version 7 - gtk2-perl support, no longer use info-look for symbol at point
;; Version 8 - fix perl Glib::G_FOO and UIManager symbol munging
;; Version 9 - leave cache uninitialized if C-g interrupt during cache build,
;;             fix camel case VScrollbar symbol munging
;; Version 10 - fix preferring .devhelp2 files over .devhelp when not .gz

;;; Code:

(require 'browse-url)

;;;###autoload
(defgroup gtk-lookup nil
  "GTK/GNOME documentation lookup."
  :prefix "gtk-lookup-"
  :group 'languages ;; same as info-look
  :link  '(url-link
           :tag "gtk-look.el home page"
           "http://www.geocities.com/user42_kevin/gtk-look/index.html"))

(defvar gtk-lookup-cache 'uninitialized
  "Cache of targets for `gtk-lookup-symbol'.
The current format is (NAME . (BASE . LINK)), where NAME is a
function or type string, and BASE and LINK will be concatenated
to make a URL.  BASE and LINK are separate to save a little
memory since the BASE part is shared by all the links in one
manual.  Being an alist means this can be passed to
`completing-read' and friends.

If `gtk-lookup-cache' is not yet initialized the value is the
symbol `uninitialized'.  `gtk-lookup-cache-init' should be used
to ensure it's initialized.")

(defvar gtk-lookup-history nil
  "Symbols previously looked up with `gtk-lookup-symbol'.")

(defun gtk-lookup-reset ()
  "Discard data cached for `gtk-lookup-symbol'.
This can be used to get newly installed documents recognised."
  (interactive)
  (setq gtk-lookup-cache 'uninitialized))

;; note this defcustom is after gtk-lookup-reset so the :set method here can
;; call gtk-lookup-reset when setting the initial value
(defcustom gtk-lookup-devhelp-indices
  '( ;; usual place (see /usr/share/doc/devhelp-common/README
    "/usr/share/gtk-doc/html/*/*.devhelp*"
    ;; possible locally installed stuff
    "/usr/local/share/gtk-doc/html/*/*.devhelp*")
  "List of devhelp index files containing GTK/GNOME documentation.
Shell wildcards like \"*.devhelp\" can be used, and gzip \".gz\"
compressed files are allowed.

Usually these files are under /usr/share/gtk-doc/html, and
possibly /usr/local/share/gtk-doc.

If you change this variable you should call `gtk-lookup-reset' to
clear previously cached data.  This is done automatically from
the `customize' interface."

  :set (lambda (sym val)
         (custom-set-default sym val)
         (gtk-lookup-reset))
  :type '(repeat string)
  :group 'gtk-lookup)


(defmacro gtk-lookup-with-auto-compression (&rest body)
  "Evaluate BODY forms with `auto-compression-mode' enabled.
If `auto-compression-mode' is on, if it isn't already, and then
put back to its original setting when BODY returns.  The return
value is the last form in BODY."

  (if (fboundp 'with-auto-compression-mode)
      ;; emacs22
      `(with-auto-compression-mode ,@body)
    
    ;; emacs21 or xemacs21
    ;; could use the `auto-compression-mode' variable to get the state, but
    ;; it's not in xemacs21
    `(let ((gtk-lookup-with-auto-compression--old-state
            (and (fboundp 'jka-compr-installed-p) ;; if jka-compr loaded
                 (jka-compr-installed-p))))

       ;; turn on if not already on
       ;; xemacs21 has a toggle-auto-compression which takes a "no message"
       ;; arg, but not emacs21
       (if (not gtk-lookup-with-auto-compression--old-state)
           (auto-compression-mode 1))

       (unwind-protect
           (progn ,@body)
         ;; turn off again if it was off before
         (if (not gtk-lookup-with-auto-compression--old-state)
             (auto-compression-mode -1))))))

(defun gtk-lookup-cache-init ()
  "Initialize `gtk-lookup-cache', if not already done.
The return is the `gtk-lookup-cache' list."
  (when (eq gtk-lookup-cache 'uninitialized)
    ;; build in `result' and only after that set gtk-lookup-cache, so we
    ;; don't leave a half built cache if killed part-way through
    (let ((result nil)
          (found nil))
      (gtk-lookup-with-auto-compression
       (with-temp-buffer
         (let ((filelist
                ;; `file-truename' here and `remove' below will eliminate
                ;; any duplicate filenames arising from symlinks or repeat
                ;; matches of wildcards in gtk-lookup-devhelp-indices
                (sort (mapcar 'file-truename
                              (apply 'append
                                     (mapcar 'file-expand-wildcards
                                             gtk-lookup-devhelp-indices)))
                      'string<)))

           ;; if there's a .devhelp2 then don't look at the old .devhelp
           (dolist (filename filelist)
             (when (string-match "\\(.*\\)\\.devhelp2\\(\\.gz\\)?\\'"
                                 filename)
               (let ((base (match-string 1 filename)))
                 (setq filelist (remove (concat base ".devhelp")
                                        filelist))
                 (setq filelist (remove (concat base ".devhelp.gz")
                                        filelist)))))

           (while filelist
             (let ((filename (car filelist)))
               (message "Processing %s" filename)
               (setq found t)
               (let ((base (concat "file://" (file-name-directory filename))))
                 ;; In Emacs 21.3 jka-compr doesn't erase the buffer
                 ;; properly under the "replace" argument to
                 ;; insert-file-contents, so use erase-buffer instead.
                 ;; (Fixed in Emacs 22.)
                 (erase-buffer)
                 (insert-file-contents filename)

                 ;; "<function ...>" is devhelp 1 format
                 (while (re-search-forward "<function name=\"\\(struct \\|union \\|enum \\)?\\([a-zA-Z0-9_-]+\\)[ ()]*\" link=\"\\([^\"]+\\)\"/>"
                                           (point-max) t)
                   (setq result (cons (cons (match-string 2)
                                            (cons base (match-string 3)))
                                      result)))

                 ;; "<keyword ...>" is devhelp 2 format
                 ;; the name field can be
                 ;;     "enum foo"
                 ;;     "foo()"
                 ;;     "foo ()"
                 ;;
                 ;; the type field is empty for ordinary index entries like
                 ;; "Build Requirements" etc, so exclude those by matching
                 ;; only particular types
                 ;;
                 (goto-char (point-min))
                 (while (re-search-forward "<keyword type=\"\\(enum\\|function\\|macro\\|struct\\|typedef\\|union\\|variable\\)\" name=\"\\([^\"]*\\)\" link=\"\\([^\"]+\\)\""
                                           (point-max) t)
                   (let ((name (match-string 2))
                         (link (match-string 3)))

                     ;; lose leading "enum" or "union" from name
                     (if (string-match "\\`\\(enum\\|struct\\|union\\) \\(.*\\)"
                                       name)
                         (setq name (match-string 2 name)))

                     ;; lose trailing "()" or " ()" on name for functions
                     (if (string-match "\\`\\(.*?\\) ?()\\'" name)
                         (setq name (match-string 1 name)))

                     (setq result (cons (cons name (cons base link))
                                        result)))))

               (setq filelist (remove filename filelist)))))))
      (unless found
        (if (fboundp 'display-warning) ;; not in emacs21
            (display-warning 'gtk-look "No devhelp files found")
          (message "No devhelp files found")))
      (setq gtk-lookup-cache result)))
  gtk-lookup-cache)

(defun gtk-lookup-string-suffix-ci-p (suff str)
  "Return true if string SUFF is a suffix of STR, ignoring case."
  (and (>= (length str) (length suff))
       (if (fboundp 'compare-strings) ;; not in xemacs21
           (eq t (compare-strings str (- (length str) (length suff)) nil
                                  suff nil nil t))
         (setq suff (upcase suff))
         (setq str (upcase str))
         (string-equal suff
                       (substring str (- (length str) (length suff)))))))

(defun gtk-lookup-symbol-method-candidates (method)
  "Return a list of Gtk symbols (strings) having METHOD as a suffix.
For example \"set_parent\" gives a list
\(\"gtk_widget_set_parent\" \"gnome_dialog_set_parent\" ...).

The method must be after a \"_\" separator, so for instance
\"parent\" doesn't match \"gtk_widget_unparent\"."

  (setq method (concat "_" method)) ;; at _ boundary
  (let (ret)
    (dolist (elem (gtk-lookup-cache-init) ret)
      (let ((name (car elem)))
        (if (gtk-lookup-string-suffix-ci-p method name)
            (setq ret (cons name ret)))))))

(defun gtk-lookup-canonicalize-symbol (str)
  "Return canonicalized Gtk symbol STR.
Various transformations are applied to transform forms from
Gtk2-Perl, Guile-Gtk and Guile-Gnome into C names.  For example
\"gdk-keyval-to-lower\" becomes \"gdk_keyval_to_lower\", or
\"Gtk2::TreeStore->new\" becomes \"gtk_tree_store_new\"."

  (when str
    (let ((case-fold-search nil))
      ;; note xemacs21 replace-match doesn't take a "subexp" arg when
      ;; replacing in a string (only in a buffer)

      ;; gtk2-perl "Glib::G_PRIORITY_LOW" -> "G_PRIORITY_LOW", to avoid a
      ;; doubling to "g_G_..."
      (if (string-match "\\`Glib::\\(G_\\)" str)
          (setq str (replace-match "\\1" t nil str)))

      ;; gtk2-perl "Gtk2::Gdk::GDK_PRIORITY_EVENTS" -> "GDK_PRIORITY_EVENTS",
      ;; to avoid a doubling to "gdk_GDK_..."
      (if (string-match "\\`Gtk2::Gdk::\\(GDK_\\)" str)
          (setq str (replace-match "\\1" t nil str)))

      ;; gtk2-perl "Gtk2::GTK_PRIORITY_RESIZE" -> "GTK_PRIORITY_RESIZE", to
      ;; avoid a doubling to "gtk_GTK_..."
      (if (string-match "\\`Gtk2::\\(GTK_\\)" str)
          (setq str (replace-match "\\1" t nil str)))

      ;; gtk2-perl "Glib" -> "G"
      (if (string-match "\\`\\(Glib\\)\\(::\\|->\\)" str)
          (setq str (replace-match "G\\2" t nil str)))
      ;; gtk2-perl "Gtk2::Gdk" and "Gtk2::Pango" lose "Gtk2" part
      (if (string-match "\\`\\(Gtk2::\\)\\(Pango\\|Gdk\\)" str)
          (setq str (replace-match "\\2" t nil str)))
      ;; gtk2-perl "Gtk2" -> "Gtk"
      (if (string-match "\\`\\(Gtk2\\)\\(::\\|->\\)" str)
          (setq str (replace-match "Gtk\\2" t nil str)))

      ;; guile-gnome classes "<gtype-instance>" -> "gtypeInstance"
      ;; base types per gtype-name->scheme-name-alist in utils.scm
      (when (string-match "\\`<\\(.*\\)>\\'" str)
        (setq str (match-string 1 str))
        (while (string-match "\\(-\\)\\(.\\)" str)
          (setq str (replace-match (upcase (match-string 2 str)) t t str))))
      ;; guile-gnome "gtype:gtype" -> "G_TYPE_gtype"
      ;; and "gtype:gboolean" -> "G_TYPE_boolean" by stripping the "g" if
      ;; there's no match with it, but a match without
      (when (string-match "\\`\\(gtype:\\)g?" str)
        (let ((alt (replace-match "G_TYPE_" t t str)))
          (setq str (replace-match "G_TYPE_g" t t str))
          (gtk-lookup-cache-init)
          (and (not (assoc-ignore-case str gtk-lookup-cache))
               (assoc-ignore-case alt gtk-lookup-cache)
               (setq str alt))))

      (if (string-match "[_-]" str)
          (progn
            ;; function or constant

            ;; gtk2-perl camel case class like "TreeStore" -> "Tree_Store"
            (while (string-match "\\([a-z]\\)\\([A-Z]\\)" str)
              (setq str (replace-match "\\1_\\2" t nil str)))

            ;; gtk2-perl camel case like "UIManager" -> "UI_Manager"
            ;; but only two or more like UI, a single VScrollbar unchanged
            (while (string-match "\\([A-Z]\\{2,\\}\\)\\([A-Z][a-z]\\)" str)
              (setq str (replace-match "\\1_\\2" t nil str)))

            ;; component separators become "_"
            ;;    "-"   lisp
            ;;    "->"  gtk2-perl
            ;;    "::"  gtk2-perl
            (while (string-match "->\\|-\\|::" str)
              (setq str (replace-match "_" t t str))))

        ;; one word class name

        ;; gtk2-perl "::" separators eg. "Gtk::Object" -> "GtkObject",
        ;; including subclassing forms like "Gtk::Label::" -> "GtkLabel",
        (while (string-match "::" str)
          (setq str (replace-match "" t t str))))))
  
  str)

(defun gtk-lookup-symbol-bounds-of-thing-at-point ()
  "Find the bounds of a `gtk-lookup-symbol' symbol at point.
The return is a pair (BEG . END) of buffer positions, or nil if
point is not at or within a symbol."

  ;; For perl style "Gtk2::Foo->bar" demand the left side start with a
  ;; capital letter like "Gtk2::Label->new", so as to distinguish it from a
  ;; method call like "$label->set_text".  For the latter the return is just
  ;; the "set_text" part (when point is with that "set_text").
  ;;
  ;; The desired match is the one earliest in the buffer which covers point.
  ;; `re-search-backwards' is no good for that, as it stops at the first
  ;; match, not the earliest possible.  `thing-at-point-looking-at' is
  ;; better, but the optional "(...)?" perl class part ends up with only a
  ;; partial match (like only the "Store" part of "TreeStore->"), not the
  ;; biggest surrounding point.  So the strategy is to look from the
  ;; beginning of the line for the first which covers point.
  ;;
  (save-excursion
    (let ((case-fold-search nil)
          (orig-point (point))
          (re "\\([A-Z][a-zA-Z0-9_:]*[a-zA-Z0-9_]->\\)?[a-zA-Z_][a-zA-Z0-9_:-]*[a-zA-Z0-9]\\|<[a-zA-Z0-9_-]+>"))
      (beginning-of-line)
      (and (re-search-forward re nil t)
           (progn
             (while (< (match-end 0) orig-point)
               (re-search-forward re nil t))
             t)
           (<= (match-beginning 0) orig-point)
           (cons (match-beginning 0) (match-end 0))))))

(put 'gtk-lookup-symbol 'bounds-of-thing-at-point
     'gtk-lookup-symbol-bounds-of-thing-at-point)

(defun gtk-lookup-symbol-interactive-arg ()
  "Symbol argument reading for interactive `gtk-lookup-symbol'.
Return a list (SYMBOL) which is the user-selected symbol name."
  (let* ((default (gtk-lookup-canonicalize-symbol
                   (thing-at-point 'gtk-lookup-symbol)))
         (completion-ignore-case t)
         (enable-recursive-minibuffers t)
         (minibuffer-setup-hook minibuffer-setup-hook)
         (candidates (and default
                          (gtk-lookup-symbol-method-candidates default))))
    (cond ((= 1 (length candidates))
           ;; one method match, offer full name as the default
           (setq default (car candidates)))
          (candidates
           ;; two or more method matches, present a completions buffer
           (add-hook 'minibuffer-setup-hook
                     (lambda ()
                       (with-output-to-temp-buffer "*Completions*"
                         (display-completion-list candidates))))))

    (let ((symbol (gtk-lookup-canonicalize-symbol
                   (completing-read
                    (if default
                        (format "Describe symbol (default %s): " default)
                      "Describe symbol: ")
                    (gtk-lookup-cache-init)
                    nil ;; predicate
                    nil ;; require-match
                    nil ;; initial-input
                    'gtk-lookup-history
                    default))))
      (list (or symbol default "")))))

;;;###autoload
(defun gtk-lookup-symbol (symbol)
  "Lookup Gtk/Gnome documentation for SYMBOL.
SYMBOL is a string, the name of a function, variable, type, etc,
in Gtk, Gnome, and related libraries like Pango.  The symbol is
sought in \"devhelp\" indexes (see `gtk-lookup-devhelp-indices'
for their location), and the HTML page it points to is displayed.

The lookup tries first case-sensitively, then insensitively, for
ease of use when typing in a name.

When called interactively, a SYMBOL is prompted for, with
completions from the available documentation.  The default is the
function, variable, type, etc at point.  Transformations are
applied (making a C name) designed to support

    * Gtk2-Perl    (http://gtk2-perl.sourceforge.net/)
    * Guile-Gtk    (http://www.gnu.org/software/guile-gtk/)
    * Guile-Gnome  (http://www.gnu.org/software/guile-gnome/)

For example with point on a Perl call like \"Gtk2::Label->new\"
the default offered is \"gtk_label_new\".  This is independent of
the major mode, so you can have code in one style and comments in
another.  And if `browse-url' displays in a buffer you can even
lookup from the browser buffer for symbols without HTML links
\(eg. cross references from Gtk to Pango).

When point is on a \"method\" name like just \"set_size_request\"
in Gtk2-Perl or Guile-Gnome, the default is expanded to the full
name like \"gtk_widget_set_size_request\".  If there's multiple
candidates then a *Completions* window is presented which you can
switch to with \\<minibuffer-local-completion-map>\\[switch-to-completions] and select from in the usual way.

`browse-url' is used to display the documentation.  If it uses an
Emacs buffer (eg. `w3m') then it's put in an \"other window\"
below the current buffer, like `info-lookup' does for Info docs.
You can customize `browse-url-browser-function' to choose the
viewer, and with some elisp the regexp forms there can let you
have one viewer for Gtk \"file:///usr/share/gtk-doc/html/...\"
documents and another for other things.

The completing-read used for the symbol prompt normally has a
large set of symbols and you might like to try one of the
completions add-ons like Icicles to help searching or browsing."

  (interactive (gtk-lookup-symbol-interactive-arg))
  (gtk-lookup-cache-init)
  (let ((entry (or (assoc symbol gtk-lookup-cache) ;; exact match preferred
                   (assoc-ignore-case symbol gtk-lookup-cache))))
    (or entry
        (error "Unknown symbol %s" symbol))
    (gtk-lookup-browse-url-other-window (concat (cadr entry) (cddr entry)))))

(defun gtk-lookup-browse-url-other-window (url)
  "`browse-url' but in an \"other-window\" if it uses an Emacs window."

  ;; this convoluted code divines the type of browser `browse-url' invokes:
  ;; perhaps an external program in its own X window, perhaps something in
  ;; an emacs buffer.  And when in a buffer it might switch to an "other
  ;; window" itself or just use the current window; and perhaps the current
  ;; buffer (and window) is already the browser buffer
  ;;
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((orig-win-conf (current-window-configuration))
        (orig-buffer   (current-buffer))
        (orig-window   (selected-window))
        (dummy-buf     (get-buffer-create
                        "*gtk-lookup-browse-url-other-window--dummy-buffer*")))
    (switch-to-buffer-other-window dummy-buf)
    (let ((other-window (get-buffer-window dummy-buf)))
      (select-window other-window)
      (browse-url url)

      (cond ((and (eq dummy-buf (window-buffer other-window))
                  (eq orig-buffer (window-buffer orig-window)))
             ;; browse-url didn't touch the buffers, it left the original
             ;; and dummy current, so it's an external window system
             ;; program; put window configs all back how they were
             (set-window-configuration orig-win-conf))

            ((eq orig-buffer (window-buffer other-window))
             ;; browse-url has changed dummy-buf to orig-buf in the
             ;; other-window, which means we were in the browser buffer
             ;; already and shouldn't have split with "other window"; so put
             ;; window configs back how they were, but don't change point in
             ;; the browser buffer as that's the new document position
             (let ((point (window-point other-window)))
               (set-window-configuration orig-win-conf)
               (with-current-buffer orig-buffer
                 (goto-char point))))

            (t
             ;; browse-url has selected a buffer; but it might have done
             ;; "other window" itself (eg. w3m-browse-url does); don't let
             ;; two "other window" invocations leave our original buffer
             ;; at the bottom and the browser at the top, instead force
             ;; our orig-window back to orig-buffer, and let the other
             ;; window we made show the browser buffer
             (let ((browser-buffer (window-buffer other-window)))
               (select-window other-window)
               (switch-to-buffer browser-buffer)
               (select-window orig-window)
               (switch-to-buffer orig-buffer)))))

    (kill-buffer dummy-buf)))

(provide 'gtk-look)

;;; gtk-look.el ends here
