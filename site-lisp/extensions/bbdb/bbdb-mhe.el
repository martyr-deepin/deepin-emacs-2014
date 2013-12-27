;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991 Todd Kaufmann <toad@cs.cmu.edu>
;;; Interface to mh-e version 3.7 or later (modeled after bbdb-rmail).
;;; Created  5-Mar-91;
;;; Modified: 28-Jul-94 by Fritz Knabe <knabe@ecrc.de>
;;;                        Jack Repenning <jackr@dblues.wpd.sgi.com>

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
;; $Id: bbdb-mhe.el,v 1.62 2005/08/02 18:11:21 waider Exp $
;;

(eval-and-compile
  (require 'bbdb)
  (require 'bbdb-com)
  (require 'mail-utils)
  ;; We advise several mh-e functions
  (require 'mh-e)
  (if (fboundp 'mh-version)
      (require 'mh-comp))              ; For mh-e 4.x
  (require 'advice))

(defmacro bbdb/mh-cache-key (message)
  "Return a (numeric) key for MESSAGE"
  (`(let* ((attrs (file-attributes (, message)))
           (status-time (nth 6 attrs))
           (status-time-2 (cdr status-time))
           (inode (nth 10 attrs)))
      (logxor (if (integerp inode) ;; if inode is larger than an emacs int,
                  inode               ;; it's returned as a dotted pair
                (car inode))
              (car status-time)
              ;; We need the following test because XEmacs returns the
              ;; status time as a dotted pair, whereas FSF and Epoch
              ;; return it as list.
              (if (integerp status-time-2)
                  status-time-2
                (car status-time-2))))))

;;;###autoload
(defun bbdb/mh-update-record (&optional offer-to-create)
  "Returns the record corresponding to the current MH message, creating or
modifying it as necessary.  A record will be created if
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (save-excursion
    (and mh-show-buffer (set-buffer mh-show-buffer))
    (if bbdb-use-pop-up
        (bbdb/mh-pop-up-bbdb-buffer offer-to-create)
      (let ((msg (bbdb/mh-cache-key buffer-file-name))
            records record)
        (if (eq msg 0) (setq msg nil))  ; 0 could mean trouble; be safe.
        (setq records (bbdb-message-cache-lookup msg))
        (if records
            (car records)
          (let ((from (bbdb/mh-get-field "^From[ \t]*:")))
            (if (or (string= "" from)
                    (string-match (bbdb-user-mail-names)
                                  (mail-strip-quoted-names from)))
                ;; if logged-in user sent this, use recipients.
                (progn
                  (setq from (bbdb/mh-get-field "^To[ \t]*:"))
                  (if (or (string= "" from)
                          (string-match (bbdb-user-mail-names)
                                        (mail-strip-quoted-names from)))
                      (setq from nil))))
            (if from
                (setq record
                      (bbdb-annotate-message-sender
                       from t
                       (or (bbdb-invoke-hook-for-value bbdb/mail-auto-create-p)
                           offer-to-create)
		       ;; ugh. what the hell?
                       (or offer-to-create
			   (bbdb-invoke-hook-for-value bbdb/mail-auto-create-p)))))
	    (if (and msg record) (bbdb-encache-message msg (list record)))
	    ;; return one record
            record))))))

;;;###autoload
(defun bbdb/mh-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive (list (if bbdb-readonly-p
                         (error "The Insidious Big Brother Database is read-only.")
                         (read-string "Comments: "))))
  (mh-show)
  (let ((b (current-buffer))
        (p (point)))
    (set-buffer mh-show-buffer)
    (bbdb-annotate-notes (bbdb/mh-update-record t) string 'notes replace)
    (set-buffer b)
    (goto-char p)))


(defun bbdb/mh-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (mh-show)
  (let ((b (current-buffer))
        (p (point)))
    (set-buffer mh-show-buffer)
    (let (bbdb-electric-p (record (or (bbdb/mh-update-record t) (error ""))))
      (bbdb-display-records (list record))
      (if arg
          (bbdb-record-edit-property record nil t)
        (bbdb-record-edit-notes record t)))
    (set-buffer b)
    (goto-char p)))


;;;###autoload
(defun bbdb/mh-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (mh-show)
  (let ((b (current-buffer))
        (p (point)))
    (set-buffer mh-show-buffer)
    (let ((record (bbdb/mh-update-record t)))
      (if record
          (bbdb-display-records (list record))
        (error "unperson")))
    (set-buffer b)
    (goto-char p)))


(defun bbdb/mh-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the MH window,
displaying the record corresponding to the sender of the current message."
  (bbdb-pop-up-bbdb-buffer
    (function (lambda (w)
      (let ((b (current-buffer)))
        (set-buffer (window-buffer w))
        (prog1 (eq major-mode 'mh-folder-mode)
          (set-buffer b))))))
  (let ((bbdb-gag-messages t)
        (bbdb-use-pop-up nil)
        (bbdb-electric-p nil))
    (let ((record (bbdb/mh-update-record offer-to-create)))
      (bbdb-display-records (if record (list record) nil)
                            bbdb-pop-up-display-layout)
      record)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is a more strict version of mh-get-field which takes an regexp

(defun bbdb/mh-get-field (field)
  ;; Find and return the value of field FIELD (regexp) in the current buffer.
  ;; Returns the empty string if the field is not in the message.
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (cond ((not (re-search-forward field nil t)) "")
          ((looking-at "[\t ]*$") "")
          (t (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
           (let ((field (buffer-substring (match-beginning 1) (match-end 1)))
                 (end-of-match (point)))
             (forward-line)
             (while (looking-at "[ \t]") (forward-line 1))
             (backward-char 1)
             (if (<= (point) end-of-match)
                 field
                 (format "%s%s" field
                         (buffer-substring end-of-match (point)))))))))

(defadvice mh-process-commands (after mh-bbdb-process act)
  (bbdb-offer-save))

(defadvice mh-send (before mh-bbdb-send act)
  (interactive (list
                (bbdb-read-addresses-with-completion "To: ")
                (bbdb-read-addresses-with-completion "Cc: ")
                (read-string "Subject: "))))

(defadvice mh-send-other-window (before mh-bbdb-send-other act)
  (interactive (list
                (bbdb-read-addresses-with-completion "To: ")
                (bbdb-read-addresses-with-completion "Cc: ")
                (read-string "Subject: "))))

(defadvice mh-forward (before mh-bbdb-forward act)
  (interactive (list (bbdb-read-addresses-with-completion "To: ")
                     (bbdb-read-addresses-with-completion "Cc: ")
                     (if current-prefix-arg
                         (mh-read-seq-default "Forward" t)
                       (mh-get-msg-num t)))))

(defadvice mh-redistribute (before mh-bbdb-redist act)
  (interactive (list
                (bbdb-read-addresses-with-completion "Redist-To: ")
                (bbdb-read-addresses-with-completion "Redist-Cc: ")
                (mh-get-msg-num t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mail from bbdb-mode using mh

;; these redefine the bbdb-send-mail functions to use mh-send.

;;; Install bbdb into mh-e's show-message function

;;;###autoload
(defun bbdb-insinuate-mh ()
  "Call this function to hook BBDB into MH-E."
  (define-key mh-folder-mode-map ":" 'bbdb/mh-show-sender)
  (define-key mh-folder-mode-map ";" 'bbdb/mh-edit-notes)
  (define-key mh-letter-mode-map "\M-;" 'bbdb-complete-name)
  (add-hook 'mh-show-hook 'bbdb/mh-update-record)
  (define-key mh-letter-mode-map "\e\t" 'bbdb-complete-name))

(provide 'bbdb-mhe)
