;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to RMAIL.  See bbdb.texinfo.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
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
;; $Id: bbdb-rmail.el,v 1.68 2005/09/05 17:13:18 waider Exp $
;;

(require 'bbdb)
(require 'bbdb-com)
(require 'rmail)
(require 'rmailsum)
(require 'mailheader)


;;;###autoload
(defun bbdb/rmail-update-record (&optional offer-to-create)
  (let ((bbdb-get-only-first-address-p)
        (records (bbdb/rmail-update-records offer-to-create)))
    (if records (car records) nil)))

(defun bbdb/rmail-get-header-content( header-field buf )
  "Pull HEADER-FIELD out of BUF's mail header.
BUF is actually the rmail buffer from which the current message should
be extracted."
  (save-excursion
    (set-buffer buf)
    (save-restriction
      (rmail-narrow-to-non-pruned-header)
      (let ((headers (mail-header-extract))
            (header (intern-soft (downcase header-field))))
        (mail-header header headers)))))

(defun bbdb/rmail-new-flag( buf )
  "Returns t if the current message in buffer BUF is new."
  (rmail-message-labels-p rmail-current-message ", ?\\(unseen\\),"))

(defcustom bbdb/rmail-update-records-mode
  '(if (bbdb/rmail-new-flag rmail-buffer) 'annotating 'searching)
  "RMAIL-specific version of `bbdb-update-records-mode', which see."
  :group 'bbdb-mua-specific-rmail
  :type '(choice (const :tag "annotating all messages"
                        annotating)
                 (const :tag "annotating no messages"
                        searching)
                 (const :tag "annotating only new messages"
                        (if (bbdb/rmail-new-flag rmail-buffer) 'annotating 'searching))
                 (sexp  :tag "user defined")))

;;;###autoload
(defun bbdb/rmail-update-records (&optional offer-to-create)
  "Returns the records corresponding to the current RMAIL emssage,
creating or modifying them as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil or if OFFER-TO-CREATE is true, and
the user confirms the creation.

The variable `bbdb/rmail-update-records-mode' controls what actions
are performed and it might override `bbdb-update-records-mode'.

When hitting C-g once you will not be asked anymore for new people
listed n this message, but it will search only for existing records.
When hitting C-g again it will stop scanning."
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer)
    (error "Not in an rmail buffer"))
  (if rmail-current-message
      (let ((bbdb/rmail-offer-to-create offer-to-create)
            cache records)

        (if (not bbdb/rmail-offer-to-create)
            (setq cache (bbdb-message-cache-lookup
                         rmail-current-message)))

        (if cache
            (setq records (if bbdb-get-only-first-address-p
                              (list (car cache))
                            cache))

          (let ((bbdb-update-records-mode (or
                                           bbdb/rmail-update-records-mode
                                           bbdb-update-records-mode)))
            (setq records (bbdb-update-records
                           (bbdb-get-addresses
                            bbdb-get-only-first-address-p
                            ;; uninteresting-senders
                            user-mail-address
                            'bbdb/rmail-get-header-content
                            rmail-buffer)
                           bbdb/mail-auto-create-p
                           offer-to-create))

            (bbdb-encache-message rmail-current-message records)))
        records))
  )

;;;###autoload
(defun bbdb/rmail-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive (list (if bbdb-readonly-p
                         (error "The Insidious Big Brother Database is read-only.")
                         (read-string "Comments: "))))
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer))
  (bbdb-annotate-notes (bbdb/rmail-update-record t) string 'notes replace))

(defun bbdb/rmail-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (let ((record (or (bbdb/rmail-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
        (bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))


;;;###autoload
(defun bbdb/rmail-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (if (and (boundp 'rmail-buffer) rmail-buffer)
      (set-buffer rmail-buffer))
  (let ((record (bbdb/rmail-update-record t)))
    (if record
        (bbdb-display-records (list record))
        (error "unperson"))))

(defun bbdb/rmail-pop-up-bbdb-buffer ( &optional offer-to-create )
  "Make the *BBDB* buffer be displayed along with the RMAIL window(s).
Displays the records corresponding to the sender respectively
recipients of the current message.
See `bbdb/rmail-get-addresses-headers' and
'bbdb-get-only-first-address-p' for configuration of what is being
displayed."
  (save-excursion
    (let ((bbdb-gag-messages t)
          (bbdb-electric-p nil)
          (records (bbdb/rmail-update-records offer-to-create))
          (bbdb-buffer-name bbdb-buffer-name))

      (when (and bbdb-use-pop-up records)
        (bbdb-pop-up-bbdb-buffer
         (function (lambda (w)
                     (let ((b (current-buffer)))
                       (set-buffer (window-buffer w))
                       (prog1 (eq major-mode 'rmail-mode)
                         (set-buffer b))))))

        ;; Always update the records; if there are no records, empty
        ;; the BBDB window. This should be generic, not MUA-specific.
        (bbdb-display-records records bbdb-pop-up-display-layout))

      (when (not records)
        (bbdb-undisplay-records)
        (if (get-buffer-window bbdb-buffer-name)
            (delete-window (get-buffer-window bbdb-buffer-name)))))))

;;;###autoload
(defun bbdb-insinuate-rmail ()
  "Call this function to hook BBDB into RMAIL."
  (define-key rmail-mode-map ":" 'bbdb/rmail-show-sender)
  (define-key rmail-mode-map ";" 'bbdb/rmail-edit-notes)
  (define-key rmail-summary-mode-map ":" 'bbdb/rmail-show-sender)
  (define-key rmail-summary-mode-map ";" 'bbdb/rmail-edit-notes)

  (add-hook 'rmail-show-message-hook 'bbdb/rmail-pop-up-bbdb-buffer)

  ;; We must patch into rmail-only-expunge to clear the cache, since
  ;; expunging a message invalidates the cache (which is based on
  ;; message numbers).
  (defadvice rmail-only-expunge (before bbdb/rmail-only-expunge)
    "Invalidate BBDB cache before expunging."
    (setq bbdb-message-cache nil))

  ;; Same for undigestifying.
  (or (fboundp 'undigestify-rmail-message)
      (autoload 'undigestify-rmail-message "undigest" nil t))
  (if (eq (car-safe (symbol-function 'undigestify-rmail-message)) 'autoload)
      (load (nth 1 (symbol-function 'undigestify-rmail-message))))
  (defadvice undigestify-rmail-message (before bbdb/undigestify-rmail-message)
    "Invalidate BBDB cache before undigestifying."
    (setq bbdb-message-cache nil))
  )

(provide 'bbdb-rmail)
