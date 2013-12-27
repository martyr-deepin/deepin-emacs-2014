;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1995 Jamie Zawinski <jwz@netscape.com>.
;;; Invoking BBDB from another process, via `gnudoit'.
;;; See the file bbdb.texinfo for documentation.
;;;
;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;
;; $Id: bbdb-srv.el,v 1.62 2003/12/12 14:35:57 waider Exp $

;;; This requires the `gnuserv' and `itimer' packages.
;;;
;;; To use:
;;;
;;; First, do `(gnuserv-start)' to initialize the emacs server process.
;;; If you don't know what this does, see the doc for gnuserv.el.
;;;
;;; Then, an external process may invoke `gnudoit' in the following way:
;;;
;;;     gnudoit '(bbdb-server "...all message headers..")'
;;;
;;; The bbdb-srv.perl program is a good choice for this; it takes a header
;;; block on stdin, and converts them to a lisp string, taking care to
;;; "sanitize" them so that hostile data can't take over the executing shell.
;;;
;;; The string should be a validly-formatted-and-quoted lisp string, and
;;; should contain multiple lines, which are the headers of the message for
;;; which a record should be displayed.  It should contain at least a "From:"
;;; header, or nothing will be displayed, but it should contain as many headers
;;; as your various BBDB hooks might want access to.
;;;
;;; Records will not be displayed until no record has been requested for
;;; `bbdb/srv-display-delay' seconds (default 2.)  This is to prevent rapid
;;; display of records from queueing up and swamping the emacs server process.
;;;
;;; Note that in order for this to build, itimer.el and gnuserv.el must be in
;;; the build-path. The easiest way to achieve this is to set OTHERDIR to point
;;; to the directory/ies they're in.

;;; A trivial application of this is the shell command:
;;;
;;;    echo 'From: Jamie Zawinski <jwz@netscape.com>' | bbdb-srv.perl
;;;
;;; which will cause the corresponding record to be displayed.
;;; A more interesting application of this is:
;;;
;;;    setenv NS_MSG_DISPLAY_HOOK bbdb-srv.perl
;;;
;;; which will hook BBDB up to Mozilla (Unix Netscape Mail and Netscape News
;;; versions 3.0b2 and later only.)

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-hooks)


(eval-when-compile
  (require 'mail-utils) ;; for mail-strip-quoted-names
  (require 'bbdb-gui) ;; for extents macros
  (if (featurep 'xemacs)
      ()
    (fset 'set-keymap-name 'ignore)
    (fset 'frame-lowest-window 'ignore)))

;; newer version of gnuserv requires gnuserv-compat when using FSF emacs
;; but you might be using an older version, and we can't tell until you
;; crash it...
(or (fboundp 'define-obsolete-variable-alias)
    (if (locate-library "gnuserv-compat")
        (require 'gnuserv-compat)))
(require 'gnuserv)
(require 'itimer)

(defcustom bbdb/srv-auto-create-p nil
  "*Like `bbdb/news-auto-create-p' and `bbdb/mail-auto-create-p',
but for the case where the record is being displayed by some external
process via the `gnudoit' mechanism.

If this is t, then records will automatically be created; if this is a
function name or lambda, then it is called with no arguments to decide
whether an entry should be automatically created.  You can use this to,
for example, create or not create messages which have a particular subject.

`bbdb/srv-auto-create-mail-news-dispatcher' is a good value for this --
that function will try to decide if this is a mail message or a news
message, and then run either `bbdb/news-auto-create-p' or
`bbdb/mail-auto-create-p' as appropriate."
  :group 'bbdb-utilities-server
  :type '(choice (const :tag "Don't automatically create records" nil)
         (const :tag "Automatically create records" t)
         (sexp :tag "Use function to determine record creation"
               bbdb/srv-auto-create-mail-news-dispatcher)))

(defcustom bbdb/srv-display-delay 2
  "*How long (in seconds) we must be idle before displaying a record."
  :group 'bbdb-utilities-server
  :type 'integer)

(defvar bbdb/srv-pending-headers nil)
(defvar bbdb/srv-pending-map
  (and (fboundp 'bbdb-set-extent-property)
       (condition-case nil
       (let ((m (make-sparse-keymap)))
         (set-keymap-name m 'bbdb/srv-pending-map)
         (define-key m 'button1 'bbdb/srv-pending-add)
         m)
     (error nil))))

(defun bbdb/srv-handle-headers (headers &optional create-p)
  "Display (or create) the BBDB entry corresponding to the message headers.
HEADERS should be a string containing an RFC822 header block; at least a
\"From:\" header should be provided, but others will be made available to
the various hooks (like `bbdb-notice-hook' and `bbdb/news-auto-create-p')."
  (let ((buf "*bbdb-tmp*")
    (record nil)
    (bbdb-force-dialog-boxes t) ; affects bbdb-y-or-n-p
    from)
    (save-excursion
      (set-buffer (or (get-buffer buf)
              (progn
            (setq buf (get-buffer-create buf))
            (set-buffer buf)
            (buffer-disable-undo buf)
            buf)))
      (erase-buffer)
      (insert headers "\n\n")
      (setq from (mail-fetch-field "from"))
      (if (or (null from)
          (string-match (bbdb-user-mail-names)
                (mail-strip-quoted-names from)))
      ;; if logged-in user sent this, use recipients.
      (setq from (or (mail-fetch-field "to") from)))
      (if from
      (setq record
        (bbdb-annotate-message-sender from t
                          (or create-p
                          (bbdb-invoke-hook-for-value
                           bbdb/srv-auto-create-p))
                          nil))))
    (let ((w (get-buffer-window bbdb-buffer-name)))
      (if w
      nil
    (setq w (selected-window))
    (unwind-protect
        (progn
          (if (fboundp 'frame-lowest-window)
              (select-window (frame-lowest-window)))
          (bbdb-pop-up-bbdb-buffer))
      (select-window w))
    (setq w (get-buffer-window bbdb-buffer-name))
    (if (fboundp 'set-window-dedicated-p)
        (set-window-dedicated-p w bbdb-buffer-name))))
    (cond (record
       (let ((bbdb-gag-messages t)
         (bbdb-use-pop-up nil)
         (bbdb-electric-p nil)
         (b (current-buffer)))
         (save-window-excursion ;; needed to get around XEmacs 19.15 bug?
           (bbdb-display-records (list record)) bbdb-pop-up-display-layout)
         (set-buffer b)))
      ((and from (not create-p) bbdb/srv-pending-map)
       (setq bbdb/srv-pending-headers headers)
       (save-excursion
         (set-buffer bbdb-buffer-name)
         (let ((buffer-read-only nil))
           (erase-buffer)
           (insert "\t\t\t")
           (let ((p (point))
             e)
         (insert from)
         (setq e (bbdb-make-extent p (point)))
         (bbdb-set-extent-face e 'bold)
         (bbdb-set-extent-property e 'highlight t)
         (bbdb-set-extent-property e 'keymap bbdb/srv-pending-map)
         )
           (insert "\n\n\t\t\tClick to add to BBDB.")
           ))))))

(defun bbdb/srv-pending-add ()
  (interactive "@")
  (or bbdb/srv-pending-headers (error "lost headers?"))
  (bbdb/srv-handle-headers bbdb/srv-pending-headers t))


(defvar bbdb/srv-itimer-arg nil)
(defun bbdb/srv-itimer ()
  "Used as a timer function by bbdb/srv-handle-headers-with-delay.
This invokes bbdb/srv-handle-headers with bbdb/srv-itimer-arg.
We do it this way instead of by using a lambda to start-itimer so that
we cons less."
  (defvar current-itimer)
  (if current-itimer (delete-itimer current-itimer))
  (if bbdb/srv-itimer-arg
      (bbdb/srv-handle-headers
       (prog1 bbdb/srv-itimer-arg
     (setq bbdb/srv-itimer-arg nil)))))

;;;###autoload
(defun bbdb/srv-handle-headers-with-delay (headers)
  "Just like bbdb/srv-handle-headers, but only updates every few seconds.
This is so that trying to display many records in succession won't queue them
up, but will end up only displaying a record when no displays have been
requested for a couple of seconds."
  (let* ((name "bbdb-srv")
     (itimer (get-itimer name)))
    (setq bbdb/srv-itimer-arg headers)
    (if itimer
    ;; It hasn't gone off yet; just change what it's argument will be.
    nil
      ;; else, start the timer going again.
      (start-itimer name 'bbdb/srv-itimer bbdb/srv-display-delay nil))
    nil))

;;;###autoload
(defalias 'bbdb-srv 'bbdb/srv-handle-headers-with-delay)

(autoload 'bbdb-header-start "bbdb-hooks")

;;;###autoload
(defun bbdb/srv-auto-create-mail-news-dispatcher ()
  "For use as the value of bbdb/srv-auto-create-p.
This will try to decide if this is a mail message or a news message, and then
run either bbdb/news-auto-create-p or bbdb/mail-auto-create-p as appropriate.
\(The heuristic is that news messages never have a Status or X-Mozilla-Status
header; and that mail messages never have Path headers.)"
  (let (mail-p)
    (save-excursion
      (let ((start (bbdb-header-start)))
    (set-buffer (marker-buffer start))
    (setq mail-p
          (cond ((progn (goto-char start)
                (bbdb-extract-field-value "Status"))
             t)
            ((progn (goto-char start)
                (bbdb-extract-field-value "X-Mozilla-Status"))
             t)
            ((progn (goto-char start)
                (bbdb-extract-field-value "Path"))
             nil)
            (t t)))))       ; can't tell -- guess mail.
    (bbdb-invoke-hook-for-value
     (if mail-p bbdb/mail-auto-create-p bbdb/news-auto-create-p))))


;; For caller-id stuff
;;;###autoload
(defun bbdb-srv-add-phone (phone-string &optional description record)
  (let ((phone (make-vector (if bbdb-north-american-phone-numbers-p
                                bbdb-phone-length
                              2)
                            nil)))
    (setq record (if (stringp record)
                     (or (bbdb-search-simple record "")
                         (bbdb-create-internal record nil nil nil nil nil))
                   (bbdb-completing-read-record
                    (format "Add %s to: " phone-string))))
    (if (= 2 (length phone))
        (aset phone 1 phone-string)
      (let ((newp (bbdb-parse-phone-number phone-string)))
        (bbdb-phone-set-area phone (nth 0 newp))
        (bbdb-phone-set-exchange phone (nth 1 newp))
        (bbdb-phone-set-suffix phone (nth 2 newp))
        (bbdb-phone-set-extension phone (or (nth 3 newp) 0))))
    (bbdb-phone-set-location phone
                             (or description
                                 (read-string "Phone number description: "
                                              "cid")))
    (bbdb-record-set-phones record
                            (nconc (bbdb-record-phones record) (list phone)))
    (bbdb-change-record record nil)
    (bbdb-display-records (list record))
    record))

(provide 'bbdb-srv)
