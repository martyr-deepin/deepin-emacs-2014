;;; deb-view.el --- view Debian package files with tar-mode

;; Copyright (C) 1998 Rick Macdonald <rickmacd@shaw.ca>
;; Copyright (C) 2003, 2004, 2005, 2009 Peter S Galbraith <psg@debian.org>

;; Author:     Rick Macdonald <rickmacd@shaw.ca>
;; Maintainer: Peter S. Galbraith <psg@debian.org>
;; Version: 1.15

;; This file is not part of GNU Emacs.

;; deb-view is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; deb-view is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with deb-view; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; deb-view presents the contents of debian package archive files for
;; viewing. The viewing is done with the major mode "debview", which
;; is derived from emacs tar-mode with a few enhancements for viewing
;; compressed files, HTML files and formatted man pages.  The normal
;; editing and saving features of tar-mode are not supported by
;; deb-view.

;; deb-view includes a command called deb-find which requires that you
;; have the debian distribution directories on a local or mounted
;; filesystem. Give it a string or regular expression and it presents a
;; buffer of matching deb file names.  Click with the middle mouse button
;; or press RETURN (or ^C^C) and it launches deb-view on the selected
;; file. deb-find can be configured to use locate or find, or any other
;; external command. The find method passes your search specification to
;; egrep, whereas the locate method uses your string directly.

;; deb-view extracts the control.tar.gz and data.tar.gz files from
;; debian package and presents two buffers in a derivitive of
;; tar-mode. See tar-mode for info.

;; Required programs: ar, gzip.
;; Optionally required programs: nroff for formatting man pages.
;; Optionally required programs: dpkg-deb for old-style binary .deb files.
;; Optionally required programs: w3-mode for viewing HTML pages.

;; For new-style .deb files (2.0), dpkg-deb isn't used. Therefore
;; deb-view should work on any platform with the ar command, although
;; "ar -p" doesn't seem to work for .deb files on Solaris 2.4 and 2.5.
;; It works on Solaris 2.6, SGI's IRIX 6.1 and 6.2, and Linux, of course.

;; Old-style .deb files require the dpkg-deb program. I don't know how to
;; extract control.tar.gz from these deb files, so you only get to see
;; the package control file, but nothing else such as the install scripts.
;; If you know how to get the control.tar.gz file out, let me know!
;; The data file is still viewable thanks to the "dpkg-deb --fsys-tarfile"
;; option.

;;; Installation:

;; 1) Quick test to see if you like deb-view.

;; Put this file in your home directory, and call it deb-view.el.
;; Start up emacs and do the following:
;;      ESCAPE x load-file RETURN ~/deb-view.el RETURN
;; Then, view a deb file with CTRL-d in Dired mode,
;; or execute:
;;      ESCAPE x deb-view RETURN {/full/path/of/file.deb} RETURN
;; or execute:
;;      ESCAPE : (setq deb-find-directory "/your/debian/directory") RETURN
;;      ESCAPE x deb-find RETURN {deb-file-search-string} RETURN
;;      and select a deb file to view with RETURN or middle mouse button in
;;      the search results buffer that is created. Exit this buffer with "q".

;; 2) Permanent installation.

;; When installed this way, all find-file operations (such as "f" or "v" in
;; dired-mode) will automatically recognize debian files and load deb-view
;; when required.

;; Put this file somewhere where Emacs can find it (i.e., in one of the paths
;; in your `load-path'), `byte-compile-file' it, and put the following six
;; lines (with semi-colons removed) in your ~/.emacs file (or create ~/.emacs
;; if you don't have one):
;;(autoload 'deb-find            "deb-view" "Debian Archive File Finder" t)
;;(autoload 'deb-view-mode       "deb-view" "Debian Archive File Mode" t)
;;(autoload 'deb-view            "deb-view" "Debian Archive File Viewer" t)
;;(autoload 'deb-view-dired-view "deb-view" "Debian Archive File Viewer" t)
;;(setq auto-mode-alist (append '(("\\.deb$" . deb-view-mode)) auto-mode-alist))
;;(define-key dired-mode-map     "\C-d"     'deb-view-dired-view)

;; If you're not very familiar with emacs customization, here is a simpler
;; approach. Add this line to your ~/.emacs file (or create ~/.emacs if you
;; don't have one):
;;     (load "~/deb-view.el")
;; Or, if you can put deb-view into your load-path (execute
;; "^h v load-path RETURN" to see your load-path setting)
;; then just add the following to your ~./emacs file:
;;     (require 'deb-view)

;; deb-view is mostly unobtrusive, but does bind ^d in dired to
;; deb-view-dired-view.  The "debview" mode is derived from
;; tar-mode.el using derived.el. Compared to tar-mode, debview-mode
;; binds q, N, W, and re-binds v. Also, the normal editing and saving
;; features of tar-mode are not supported by debview mode and those
;; keys are disabled.

;; 3) Configuration

;; deb-find has two variables to set. deb-find-method can be "locate" or
;; "find". Any other value will be assumed to be an external script or
;; program that you supply. If you set deb-find-method to "find" then you
;; must also set deb-find-directory to the directory containing the
;; debian distribution. The find command starts at this point. I originally
;; used the locate option, but contrary to the man page it doesn't seem to
;; understand even simple regular expressions. I prefer the find option. It
;; uses egrep and therefore understands complex regular expressions.
;; You might want to bind deb-find to a special key. I use ^C^D like this:
;; (define-key ctl-x-map "\C-d" 'deb-find)
;; Note that this key is normally the brief list-directory command, a
;; command that I never used anyway.


;;; Usage:

;; In dired, press f or e on the dired line of the .deb file to view.
;; You can also use ^d, which is actually slightly faster since the
;; deb file isn't loaded into a buffer needlessly.

;; Or, execute: ESCAPE x deb-view RETURN, and enter the .deb file name
;; at the prompt.

;; Or, execute: ESCAPE x deb-find RETURN, and enter any substring of a
;; deb file name to search for. A buffer of matches is created.
;; Launch deb-view by selecting a deb file with the middle mouse button,
;; or RETURN or ^c^c. Exit this buffer with "q".

;; You are shown two tar files in debview-mode (see tar-mode for help).
;; In the case of old .deb format files, the control info is shown
;; but not the other files of control.tar, such as install scripts.
;; Note that regular tar-mode commands e, f and RETURN show raw files
;; without any special uncompressing or formatting.
;; Additional features that deb-view adds to tar-mode:
;; q - kill both view buffers (INFO and DATA) and return to the
;;     dired buffer if that's where you executed deb-mode.
;; v - executes deb-view-tar-view instead of tar-view, with the
;;     additional smarts to uncompress .gz and .Z files for viewing.
;; N - Like in dired, formats man pages for viewing, with the
;;     additional smarts to uncompress .gz and .Z man files for viewing.
;; W - use w3-mode to view an HTML file.

;; To view files not supported by deb-view, such as graphics, use the
;; copy command ("c") to copy the file to a temp directory.  You can
;; then do what you want to the file.


;;; History:
;;

;; 1.3  - modified logic that determines old or new style Debian packages.
;;        On systems where the file command recognizes debian files, it
;;        wrongly always came up with old format.

;; 1.4  - added missing semicolons in the comments for Changelog 1.3.
;;      - fixed various spacing issues in doc strings.
;;      - disabled tar-mode keys that are not applicable to deb-view.

;; 1.5  - added an auto-mode-alist and deb-view mode so that deb-view
;;        is launched from any find-file command.
;;      - added a deb-find command that takes a search string and creates
;;        a buffer of matching deb files. ^C^C, RETURN or middle mouse button
;;        runs deb-view on the selected deb file.
;;      - added deb-view-help to "?" key in deb-view.

;; 1.6  - improved doc strings for deb-find and deb-find-method.
;;      - added (provide 'deb-view) and instructions for using
;;        (require 'deb-view).
;;      - reworked the documentation somewhat, but it's still too long.
;;      - changed the copyright notice to refer to deb-view, not Emacs.

;; 1.7  - make copy of compilation-minor-mode map rather than changing
;;        it directly. It was breaking actual compilation buffer keymaps,
;;        such as grep mode.

;; 1.8  - fixed deb-find when deb-find-method is set to "find". It wasn't
;;        adding "/*" to the end of the directory name for the find command.

;; 1.9  - Added support for handling remote deb files (ange-ftp).
;;      - reworked to use derived.el instead of messing with tar-mode
;;        directly. (Thanks to era eriksson <era@iki.fi>)

;; 1.10 2003-10-30
;;  - New maintainer: Peter S. Galbraith <psg@debian.org>
;;  - checkdoc edits.
;;  - made defvars into defcustoms.

;; 1.11 2004-01-16  Peter S. Galbraith <psg@debian.org>
;;  - Resize top (control) window to fit number of lines since it
;;    doesn't really need to be 1/2 the screen.  Thanks to Dan
;;    Jacobson for suggesting this change (Closes: #224950).

;; 1.12 2005-10-24  Peter S. Galbraith <psg@debian.org>
;;  - Output an error message if the package file is corrupted
;;    (e.g. partial download).
;;    Thanks to Dan Jacobson for suggesting this change (Closes: #235673).
;;  - deb-view-dired-view: Check if file in dired is a .deb before opening.
;;    Thanks to Dan Jacobson for suggesting this change (Closes: #273902)
;;  - deb-view-tar-view: If the file to be opned is from the INFO buffer,
;;    then open in the other (larger) window.
;;    Thanks to Dan Jacobson for suggesting this change (Closes: #321869)

;; 1.13 2006-02-02 Sven Joachim <sven_joachim@web.de>
;;    Bug fix for UTF-8 (Closes: #344260)
;;    The `call-process' and `call-process-region' use
;;    default-process-coding-system rather than coding-system-for-read.
;;    The former is set to '(mule-utf-8 . mule-utf-8) in my setup, and that
;;    caused the problem.  So the solution is to bind
;;    default-process-coding-system as well in deb-view-process

;; 1.14 2009-10-25  Peter S. Galbraith <psg@debian.org>
;;    Added support for data.tar.bz2 deb files (Closes: #457094).

;; 1.15 2009-11-02  Peter S. Galbraith <psg@debian.org>
;;    Fixed stupid bug "deb-view.el fails on own debian-el_30.9-1_all.deb",
;;    thanks to Kevin Ryde (Closes: #554039).

;; 1.16 2011-08-16  Peter S. Galbraith <psg@debian.org>
;;    Added support for data.tar.xz deb files (Closes: #637579).

;;; Code:

(defgroup deb-view nil
  "View Debian package files with tar-mode"
  :group 'tools
  :prefix "deb-view")

(defcustom deb-view-tar-uncompress-program "gzip -cd"
  "*Program to use for uncompression of .gz and .Z files in `deb-view'."
  :group 'deb-view
  :type 'string)

;; Note the following useful variable from tar-mode:
;;(defvar tar-mode-show-date nil
;; "*Non-nil means Tar mode should show the date/time of each subfile.
;;This information is useful, but it takes screen space away from file names.")

(defcustom deb-find-method "find"
  "Internal `deb-find' methods supported: locate or find.
Any other entry is assumed to be an external command.
See also the variable deb-find-directory."
  :group 'deb-view
  :type '(radio (const "find") (const "locate")))

(defcustom deb-find-directory "/usr/local/src/debian"
  "Directory to run find in when deb-find-method is \"find\"."
  :group 'deb-view
  :type 'directory)

(define-derived-mode debview-mode tar-mode "debview"
  "Major mode for debview.\n\n\\{debview-mode-map}")

;; Prohibit things that tar-mode does that deb-view doesn't:
(define-key debview-mode-map "\C-d" 'undefined)
(define-key debview-mode-map "G"    'undefined)
(define-key debview-mode-map "M"    'undefined)
(define-key debview-mode-map "O"    'undefined)
(define-key debview-mode-map "d"    'undefined)
(define-key debview-mode-map "g"    'undefined)
(define-key debview-mode-map "r"    'undefined)
(define-key debview-mode-map "u"    'undefined)
(define-key debview-mode-map "x"    'undefined)
(define-key debview-mode-map ""   'undefined)

(define-key debview-mode-map "?"    'deb-view-help)
(define-key debview-mode-map "q"    'deb-view-dired-view-cleanup)
(define-key debview-mode-map "N"    'deb-view-tar-man)
(define-key debview-mode-map "W"    'deb-view-tar-w3)
(define-key debview-mode-map "v"    'deb-view-tar-view)
(define-key debview-mode-map [up]   'tar-previous-line)
(define-key debview-mode-map [down] 'tar-next-line)
(define-key debview-mode-map "\eOA" 'tar-previous-line)
(define-key debview-mode-map "\eOB" 'tar-next-line)
(define-key debview-mode-map "\e[A" 'tar-previous-line)
(define-key debview-mode-map "\e[B" 'tar-next-line)

(defvar deb-view-dired-view-return-buffer ""
  "Return to this buffer after deb-view-dired-view-cleanup is called.")
(make-variable-buffer-local 'deb-view-dired-view-return-buffer)

(defvar deb-view-tempfile ""
  "Flag saying if the deb file is temporary (ange-ftp) and needs deleting.")

(defvar deb-view-file-name ""
  "The file name being processed by `deb-view'.")

;; You might not like the key bindings that I chose:
(if (featurep 'dired)
    (define-key dired-mode-map "\C-d" 'deb-view-dired-view)
  (add-hook
   'dired-load-hook
   (function (lambda ()
               (define-key dired-mode-map "\C-d" 'deb-view-dired-view)))))

;;;###autoload
(defun deb-view-dired-view ()
  "View Debian package control and data files.
Press \"q\" in either window to kill both buffers
and return to the dired buffer. See deb-view."
  (interactive)
  (let  ((file (dired-get-filename)))
    (if (string-match ".deb$" file)
        (deb-view file)
      (error "Not a Debian package file"))))

;;;###autoload
(defun deb-view (debfile)
  "View Debian package DEBFILE's control and data files.
Press \"q\" in either window to kill both buffers.

In dired, press ^d on the dired line of the .deb file to view.
Or, execute: ESCAPE x deb-view RETURN, and enter the .deb file name
at the prompt."
  (interactive "fdeb file to view: ")
  (if (and (or (string-match "Lucid" emacs-version)
               (string-match "XEmacs" emacs-version))
           (>= emacs-major-version 21))
      (require 'view-less)
    (require 'view))
  (require 'view)
  (if (< (nth 1 (file-attributes debfile)) 0)
      (progn
        ;; This is a remote file.
        ;; Call view-file to force ange-ftp to get it first.
        (message "deb-view remote file: %s" debfile)
        (find-file debfile))
    ;; This is a local file.
    (setq debfile (expand-file-name debfile))
    ;;(message "deb-view local file: %s" debfile)
    (setq deb-view-file-name debfile)
    (setq deb-view-tempfile nil)
    (deb-view-process debfile)))

(defun deb-view-process (debfile)
  "View Debian Archive Files for package DEBFILE."
  (let* ((deb-view-buffer-name (file-name-nondirectory deb-view-file-name))
         (info-buffer-name (concat deb-view-buffer-name "-INFO"))
         (data-buffer-name (concat deb-view-buffer-name "-DATA"))
         (info-buffer (progn (and (get-buffer info-buffer-name)
                                  (kill-buffer (get-buffer info-buffer-name)))
                             (get-buffer-create info-buffer-name)))
         (data-buffer (progn (and (get-buffer data-buffer-name)
                                  (kill-buffer (get-buffer data-buffer-name)))
                             (get-buffer-create data-buffer-name)))
         (return-buffer (current-buffer))
         (coding-system-for-read 'no-conversion)
         (default-process-coding-system '(no-conversion . no-conversion))
         file-buffer
         new-archive-format)
    (message "deb-view processing deb file %s..." deb-view-buffer-name)
    ;; info
    (setq file-buffer (get-buffer-create " *file-data*"))
    (setq new-archive-format
          (save-excursion
            (set-buffer file-buffer)
            (erase-buffer)
            (call-process shell-file-name nil t nil shell-command-switch
                          (concat "file " debfile))
            (goto-char 1)
            (if (string-match "archive" (buffer-string))
                t
              (goto-char 1)
              (if (string-match "old debian" (buffer-string))
                  nil
                t))))
    (kill-buffer file-buffer)
    (set-buffer info-buffer)
    (cond
     (new-archive-format
      ;; New deb format (archive)
      (call-process shell-file-name nil t nil shell-command-switch
                    (concat "ar -p " debfile
                            " control.tar.gz | gzip -cd"))
      (goto-char 1)
      (setq buffer-file-name (concat deb-view-file-name "-INFO"))
      (if (fboundp 'set-buffer-multibyte) (set-buffer-multibyte nil))
      (debview-mode)
      ;; Turn off view-mode in this buffer:
      (make-variable-buffer-local 'view-mode-hook)
      (add-hook
       'view-mode-hook
       (function (lambda ()
                   (view-mode -1)
                   (setq view-exit-action 'deb-view-dired-view-cleanup))))
      (message "deb-view processing deb file %s..." deb-view-buffer-name)
      (tar-next-line 1)
      (switch-to-buffer info-buffer t))
     (t
      ;; Old deb format
      (message "deb-view old dpkg binary format")
      (call-process shell-file-name nil t nil shell-command-switch
                    (concat "dpkg-deb -I " debfile))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char 1)
      (switch-to-buffer info-buffer t)
      (view-mode-enter return-buffer 'deb-view-dired-view-cleanup)))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq deb-view-dired-view-return-buffer return-buffer)
    (delete-other-windows)
    ;; data
    (set-buffer data-buffer)
    (buffer-disable-undo)
    (cond
     (new-archive-format
      (call-process "ar" nil '(t t) nil "-t" debfile)
      (goto-char 1)
      (cond
       ((re-search-forward "data.tar.gz" nil t)
        (erase-buffer)
        (call-process "ar" nil '(t t) nil "-p" debfile "data.tar.gz")
        (goto-char (point-max))
        (when (search-backward "is not a valid archive" nil t)
          (kill-buffer data-buffer)
          (kill-buffer info-buffer)
          (error "%s: Not a valid package file" deb-view-buffer-name))
        (call-process-region (point-min) (point-max) "gzip" t t nil "-cd"))
       ((and (goto-char 1)(re-search-forward "data.tar.bz2" nil t))
        (erase-buffer)
        (call-process "ar" nil '(t t) nil "-p" debfile "data.tar.bz2")
        (goto-char (point-max))
        (when (search-backward "is not a valid archive" nil t)
          (kill-buffer data-buffer)
          (kill-buffer info-buffer)
          (error "%s: Not a valid package file" deb-view-buffer-name))
        (call-process-region (point-min) (point-max) "bzip2" t t nil "-cd"))
       ((and (goto-char 1)(re-search-forward "data.tar.xz" nil t))
        (erase-buffer)
        (call-process "ar" nil '(t t) nil "-p" debfile "data.tar.xz")
        (goto-char (point-max))
        (when (search-backward "is not a valid archive" nil t)
          (kill-buffer data-buffer)
          (kill-buffer info-buffer)
          (error "%s: Not a valid package file" deb-view-buffer-name))
        (call-process-region (point-min) (point-max) "xz" t t nil "-cd"))))
     (t
      (call-process shell-file-name nil t nil shell-command-switch
                    (concat "dpkg-deb --fsys-tarfile " debfile))))
    (goto-char 1)
    (setq buffer-file-name (concat deb-view-file-name "-DATA"))
    (if (fboundp 'set-buffer-multibyte) (set-buffer-multibyte nil))
    (debview-mode)
    (message "deb-view processing deb file %s..." deb-view-buffer-name)
    (tar-next-line 1)
    (setq deb-view-dired-view-return-buffer return-buffer)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (buffer-enable-undo)
    (switch-to-buffer-other-window data-buffer)
    (if new-archive-format (other-window 1))
    (shrink-window-if-larger-than-buffer)
    (when deb-view-tempfile
      (message "deb-view deleting tempfile: %s" debfile)
      (delete-file debfile))
    (message "deb-view: ? for help. q to quit.")))

;;;###autoload
(defun deb-view-mode ()
  "View mode for Debian Archive Files."
  (interactive)
  (let ((debfile buffer-file-name)
        (return-buffer (nth 0 (buffer-list)))
        (curbuf (current-buffer)))
    (setq deb-view-file-name debfile)
    (if (< (nth 1 (file-attributes debfile)) 0)
        (progn
          (message "deb-view remote file: %s" debfile)
          (setq debfile (make-temp-name "/tmp/deb-view."))
          ;;(message "deb-view processing deb file %s..." debfile)
          (write-file debfile)
          (setq deb-view-tempfile t))
      ;;(message "deb-view local file: %s" debfile)
      (setq deb-view-tempfile nil))
    (set-buffer return-buffer)
    (kill-buffer curbuf)
    (deb-view-process debfile)))

;;;###autoload
(defun deb-find ()
  "Search for deb files.
Use the method specified by the variable deb-find-method, and collect
output in a buffer.  See also the variable deb-find-directory.

This command uses a special history list, so you can
easily repeat a `deb-find' command."
  (interactive)
  (require 'compile)
  (let* ((deb-file-string (read-from-minibuffer "deb file to find: "
                                                nil nil nil 'deb-find-history))
         (output-buffer-name "*deb-find*")
         (command (cond ((string-equal deb-find-method "locate")
                         (concat "locate '" deb-file-string
                                 "' | egrep '\.deb$'"))
                        ((string-equal deb-find-method "find")
                         (concat "find " deb-find-directory "/* | egrep '"
                                 deb-file-string "' | egrep '\.deb$'"))
                        (t
                         (concat deb-find-method " '" deb-file-string "'")))))
    (compile-internal command "Not applicable in deb-find" "deb-find" nil nil
                      (function (lambda (mode) output-buffer-name)))
    (switch-to-buffer-other-window output-buffer-name)
    (setq deb-view-find-minor-mode-map
          (copy-keymap compilation-minor-mode-map))
    (use-local-map deb-view-find-minor-mode-map)
    (define-key deb-view-find-minor-mode-map [mouse-2]
      'deb-find-mouse-deb-view)
    (define-key deb-view-find-minor-mode-map "\C-c\C-c" 'deb-find-deb-view)
    (define-key deb-view-find-minor-mode-map "\C-m"     'deb-find-deb-view)
    (define-key deb-view-find-minor-mode-map "?"        'deb-find-help)
    (define-key deb-view-find-minor-mode-map "q"        'kill-this-buffer)
    (define-key deb-view-find-minor-mode-map "\M-n" 'undefined)
    (define-key deb-view-find-minor-mode-map "\M-p" 'undefined)
    (define-key deb-view-find-minor-mode-map "\M-{" 'undefined)
    (define-key deb-view-find-minor-mode-map "\M-}" 'undefined)
    (beginning-of-buffer)
    (message "deb-view: ? for help. q to quit.")))


;;; Internal functions:

(defvar deb-view-version "1.9"
  "The version of `deb-view'.")

(defun deb-view-version ()
  "Return string describing the version of `deb-view'.
When called interactively, displays the version."
  (interactive)
  (if (interactive-p)
      (message "deb-view version %s" (deb-view-version))
    deb-view-version))

(defun deb-view-dired-view-cleanup (&optional buffer)
  "Delete the buffers created by deb-view-dired-view."
  (interactive)
  (let* ((quit-buffer (or buffer (current-buffer)))
         (bufname (buffer-name quit-buffer))
         (debfile (substring bufname 0 (- (length bufname) 5)))
         (info-buffer (get-buffer (concat debfile "-INFO")))
         (data-buffer (get-buffer (concat debfile "-DATA")))
         (ddir-buffer (save-excursion
                        (set-buffer quit-buffer)
                        deb-view-dired-view-return-buffer)))
    (delete-other-windows)
    (and (buffer-live-p info-buffer)
         (kill-buffer info-buffer))
    (and (buffer-live-p data-buffer)
         (kill-buffer data-buffer))
    (and (buffer-live-p quit-buffer)
         (kill-buffer quit-buffer))
    (and (buffer-live-p ddir-buffer)
         (switch-to-buffer ddir-buffer))))

(defun deb-find-help ()
  "Show help information for `deb-find'."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (format "deb-find mode:    version %s" (deb-view-version)))
    (princ "\n
RET     - view the deb file on this line with deb-view.
C-c C-c - view the deb file on this line with deb-view.
mouse-2 - view the deb file on this line with deb-view.
? - show deb-find-help.
q - quit deb-find.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

(defun deb-view-help ()
  "Show help information for `deb-view'."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (format "deb-view mode:    version %s" (deb-view-version)))
    (princ "
Derived from tar-mode, with additional features for viewing deb files.
Execute \"^h m\" to see tar-mode bindings.

You are shown two tar files in tar-mode (see tar-mode for help).
In the case of old .deb format files, the control info is shown
but not the other files of control.tar, such as install scripts.

Note that regular tar-mode commands e, f and RETURN show raw files
without any special uncompressing or formatting.

Additional features that deb-view adds to tar-mode:
? - show deb-view help.
q - kill both view buffers (INFO and DATA) and return to the
    dired buffer if that's where you executed deb-mode.
v - executes deb-view-tar-view instead of tar-view, with the
    additional smarts to uncompress .gz and .Z files for viewing.
N - Like in dired, formats man pages for viewing, with the
    additional smarts to uncompress .gz and .Z man files for viewing.
W - use w3-mode to view an HTML file.
These functions are also available in tar-mode on normal tar files
when deb-view is loaded.

To view files not supported by deb-view, such as graphics, use the
copy command in tar-mode (\"c\") to copy the file to a temp directory.
You can then do what you want to the file.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

(defun deb-view-tar-man ()
  "*In Tar mode, view the tar file entry on this line as a man page."
  (interactive)
  (require 'man)
  (let ((auto-mode-alist
         (append '(("\\.gz$" . deb-view-tar-uncompress-while-visiting)
                   ("\\.Z$"  . deb-view-tar-uncompress-while-visiting)
                   ) auto-mode-alist)))
    (tar-extract 'view)
    (setq buffer-read-only nil)
    (shell-command-on-region (point-min) (point-max) "nroff -man -h " t t)
    (Man-cleanup-manpage)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (message "")))

(defun deb-view-tar-uncompress-while-visiting ()
  "Temporary \"major mode\" used for .Z and .gz files, to uncompress them.
It then selects a major mode from the uncompressed file name and contents.
\(Modifed uncompress-while-visiting from uncompress.el\)"
  (interactive)
  (message "Uncompressing...")
  (let ((buffer-read-only nil))
    (shell-command-on-region (point-min) (point-max)
                             deb-view-tar-uncompress-program t))
  (message "Uncompressing...done")
  (set-buffer-modified-p nil)
  (goto-char 1))

(defun deb-view-tar-view ()
  "*In Tar mode, view the tar file entry on this line.
If the file is from the INFO buffer, then open in the other (larger) window."
  (interactive)
  (let ((auto-mode-alist
         (append '(("\\.gz$" . deb-view-tar-uncompress-while-visiting)
                   ("\\.Z$"  . deb-view-tar-uncompress-while-visiting)
                   ) auto-mode-alist)))
    (if (string-match "INFO$" buffer-file-name)
        (tar-extract-other-window)
      (tar-extract 'view))))

(defun deb-view-tar-w3 ()
  "*In Tar mode, view the tar file entry on this line as HTML with w3-mode."
  (interactive)
  (if (fboundp 'w3-preview-this-buffer)
      (let ((auto-mode-alist
             (append '(("\\.gz$" . deb-view-tar-uncompress-while-visiting)
                       ("\\.Z$"  . deb-view-tar-uncompress-while-visiting)
                       ) auto-mode-alist)))
        (tar-extract 'view)
        (rename-buffer (concat " " (buffer-name)))
        (w3-preview-this-buffer)
        (define-key w3-mode-map "q" 'deb-view-tar-w3-quit))
    (error "Sorry, you don't seem to have w3 loaded")))

(defun deb-view-tar-w3-quit ()
  "Quit WWW mode in a buffer from `deb-view'."
  (interactive)
  (let ((x w3-current-last-buffer))
    (and (fboundp 'w3-mpeg-kill-processes) (w3-mpeg-kill-processes))
    (kill-buffer (current-buffer))
    (if (and (bufferp x) (buffer-name x))
        (if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x))))
  (view-exit))

(defvar deb-find-history nil
  "History list for `deb-find' commands.")

(defvar deb-find-regexp "^/.*\.deb$"
  "Regexp for deb file names in the `deb-find' buffer.")

(defun deb-find-deb-view ()
  "Run `deb-view' in package under point."
  (interactive)
  (let ((deb-file (thing-at-point 'filename)))
    (if (and deb-file
             (string-match deb-find-regexp deb-file))
        (deb-view (thing-at-point 'filename))
      (error "No deb file on this line"))))

(defun deb-find-mouse-deb-view (event)
  "Run `deb-view' in package under mouse EVENT."
  (interactive "e")
  (pop-to-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (let ((deb-file (thing-at-point 'filename)))
    (if (and deb-file
             (string-match deb-find-regexp deb-file))
        (deb-view (thing-at-point 'filename))
      (error "No deb file on this line"))))

(provide 'deb-view)

;;; deb-view.el ends here
