;;; BBDB merge/sync framework
;;; GNU Public License to go here. This file is under GPL, thanks guys.
;;; Copyright (c) 2000 Waider

(require 'bbdb)
(require 'bbdb-com)

;;; to do:
;;; smarter phone, notes and address merging.

;;;###autoload
(defun bbdb-merge-record (new-record &optional merge-record override)
  "Generic merge function.

Merges new-record into your bbdb, using DATE to check who's more
up-to-date and OVERRIDE to decide who gets precedence if two dates
match. DATE can be extracted from a notes if it's an alist with an
element marked timestamp. Set OVERRIDE to 'new to allow the new record
to stomp on existing data, 'old to preserve existing data or nil to
merge both together. If it can't find a record to merge with, it will
create a new record. If MERGE-RECORD is set, it's a record discovered
by other means that should be merged with.

Returns the Grand Unified Record."

  (let* ((firstname (bbdb-record-firstname new-record))
         (lastname (bbdb-record-lastname new-record))
         (aka (bbdb-record-aka new-record))
         (nets (bbdb-record-net new-record))
         (addrs (bbdb-record-addresses new-record))
         (phones (bbdb-record-phones new-record))
         (company (bbdb-record-company new-record))
         (notes (bbdb-record-raw-notes new-record))
         (name (bbdb-string-trim (concat firstname " " lastname)))
         (date (if (listp notes) (cdr (assq 'timestamp notes)) nil))
         olddate)

    ;; for convenience
    (if (stringp notes)
        (setq notes (list (cons 'notes notes))))

    ;; See if we have a record that looks right, using an intertwingle
    ;; search. Could probably parameterize that.
    ;; bbdb-merge-search-function or some such.
    (if (null merge-record)
        (setq merge-record (bbdb-search-simple name nets)))

    (if merge-record
        (progn
          ;; if date is unset, set it to the existing record's date.
          (setq olddate (bbdb-record-getprop merge-record 'timestamp)
                date (or date olddate))
          ;; FIXME if date & olddate are STILL unset, set to today's date.

          ;; if the old record is actually newer, invert the sense of override
          (if (string-lessp olddate date)
              (setq override (cond ((eq 'old override) 'new)
                                   ((eq 'new override) 'old)
                                   (t nil))))

          (bbdb-record-set-firstname merge-record
           (if (null override)
               (bbdb-merge-strings (bbdb-record-firstname merge-record)
                                   firstname " ")
             (if (eq 'new override) firstname
               (bbdb-record-firstname merge-record))))

          (bbdb-record-set-lastname merge-record
           (if (null override)
               (bbdb-merge-strings (bbdb-record-lastname merge-record)
                                   lastname " ")
             (if (eq 'new override) lastname
               (bbdb-record-lastname merge-record))))

          (bbdb-record-set-company merge-record
           (if (null override)
               (bbdb-merge-strings (bbdb-record-company merge-record)
                                   company " ")
             (if (eq 'new override) company
               (bbdb-record-company merge-record))))

          (bbdb-record-set-aka
           merge-record
           (if (null override)
               (bbdb-merge-lists!
                (bbdb-record-aka merge-record)
                (if (listp aka) aka (list aka)) 'string= 'downcase)
             (if (eq 'new override) aka
               (bbdb-record-aka merge-record))))

          (bbdb-record-set-net
           merge-record
           (if (null override)
               (bbdb-merge-lists!
                (bbdb-record-net merge-record) nets 'string= 'downcase)
             (if (eq 'new override) nets
               (bbdb-record-net merge-record))))

          (bbdb-record-set-phones
           merge-record
           (if (null override)
               (bbdb-merge-lists!
                (bbdb-record-phones merge-record) phones 'equal)
             (if (eq 'new override) phones
               (bbdb-record-phones merge-record))))

          (bbdb-record-set-addresses
           merge-record
           (if (null override)
               (bbdb-merge-lists!
                (bbdb-record-addresses merge-record) addrs 'equal)
             (if (eq 'new override) addrs
               (bbdb-record-addresses merge-record))))

          ;; lifted from bbdb-com.el
          (let ((n1 (bbdb-record-raw-notes merge-record))
                (n2 notes)
                tmp
                (bbdb-refile-notes-default-merge-function ;; XXX
                 'bbdb-merge-strings))
            (or (equal n1 n2)
                (progn
                  (or (listp n1) (setq n1 (list (cons 'notes n1))))
                  (or (listp n2) (setq n2 (list (cons 'notes n2))))
                  (while n2
                    (if (setq tmp (assq (car (car n2)) n1))
                        (setcdr tmp
                                (funcall (or (cdr (assq (car (car n2))
                                                        bbdb-refile-notes-generate-alist))
                                             bbdb-refile-notes-default-merge-function)
                                         (cdr tmp) (cdr (car n2))))
                      (setq n1 (nconc n1 (list (car n2)))))
                    (setq n2 (cdr n2)))
                  (bbdb-record-set-raw-notes merge-record n1)))))

      ;; we couldn't find a record, so create one
      (setq merge-record
            (bbdb-create-internal name company nets addrs phones notes))
      ;; bite me, bbdb-create-internal
      (bbdb-record-set-firstname merge-record firstname)
      (bbdb-record-set-lastname merge-record lastname))

    ;; more general bitingness
    (if (equal (bbdb-record-firstname merge-record) "")
        (bbdb-record-set-firstname merge-record nil))
    (if (equal (bbdb-record-lastname merge-record) "")
        (bbdb-record-set-lastname merge-record nil))

    ;; fix up the in-memory copy.
    (bbdb-change-record merge-record t)
    (let ((name    (bbdb-record-name    merge-record))
          (company (bbdb-record-company merge-record)))
      (if (> (length name) 0)
          (bbdb-remhash (downcase name) merge-record))
      (if (> (length company) 0)
          (bbdb-remhash (downcase company) merge-record)))
    (bbdb-record-set-namecache merge-record nil)
    (if (or (bbdb-record-lastname merge-record)
            (bbdb-record-firstname merge-record))
        (bbdb-puthash (downcase (bbdb-record-name merge-record)) merge-record))
    (if (bbdb-record-company merge-record)
        (bbdb-puthash (downcase (bbdb-record-company merge-record))
                      merge-record))
    (bbdb-with-db-buffer
     (if (not (memq merge-record bbdb-changed-records))
         (setq bbdb-changed-records
               (cons merge-record bbdb-changed-records))))

    ;; your record, sir.
    merge-record))

;; fixme these could be a macros, I guess.
(defun bbdb-instring( s1 s2 )
;;  (and case-fold-search
;;       (setq s1 (downcase s1)
;;             s2 (downcase s2)))
  (catch 'done
    (while (>= (length s1) (length s2))
      (if (string= s2 (substring s1 0 (length s2)))
          (throw 'done t)
        (setq s1 (substring s1 1))))
    (throw 'done nil)))

(defun bbdb-merge-strings (s1 s2 &optional sep)
  "Merge two strings together uniquely.
If s1 doesn't contain s2, return s1+sep+s2."
  (cond ((or (null s1) (string-equal s1 "")) s2)
        ((or (null s2) (string-equal s2 "")) s1)
        (t (if (bbdb-instring s2 s1) s1
             (concat s1 (or sep "") s2)))))

;;;###autoload
(defun bbdb-merge-file (&optional bbdb-new override match-fun)
  "Merge a bbdb file into the in-core bbdb."
  (interactive "fMerge bbdb file: ")
  (or bbdb-gag-messages
      bbdb-silent-running
      (message "Merging %s" bbdb-new))
  ;; argh urgle private environment
  (let* ((bbdb-live-file bbdb-file)
         (bbdb-file bbdb-new)
         (bbdb-live-buffer-name bbdb-buffer-name)
         (bbdb-buffer-name "*BBDB-merge*")
         (bbdb-buffer nil) ;; hack hack
         (new-records (bbdb-records))
         (bbdb-buffer nil) ;; hack hack
         (bbdb-file bbdb-live-file)
         (bbdb-buffer-name bbdb-live-buffer-name)
         (bbdb-refile-notes-default-merge-function 'bbdb-merge-strings))

    ;; merge everything
    (mapcar (lambda(rec)
              (bbdb-merge-record rec
                                 (and match-fun
                                      (funcall match-fun rec))
                                 override))
            new-records))
  ;; hack
  (setq bbdb-buffer (or (get-file-buffer bbdb-file) nil)))

(defun bbdb-add-or-update-phone ( record location phone-string )
  "Add or update a phone number in the current record.

Insert into RECORD phone number for LOCATION consisting of
PHONE-STRING. Will automatically overwrite an existing phone entry for
the same location."
  (let* ((phone (make-vector (if bbdb-north-american-phone-numbers-p
                                 bbdb-phone-length
                               2)
                             nil)))
    (if (= 2 (length phone))
        (aset phone 1 phone-string)
      (let ((newp (bbdb-parse-phone-number phone-string)))
        (bbdb-phone-set-area phone (nth 0 newp))
        (bbdb-phone-set-exchange phone (nth 1 newp))
        (bbdb-phone-set-suffix phone (nth 2 newp))
        (bbdb-phone-set-extension phone (or (nth 3 newp) 0))))
    (bbdb-phone-set-location phone location)

    ;; "phone" now contains a suitable record
    ;; we need to check if this is already in the phones list
    (let ((phones (bbdb-record-phones record))
          phones-list)
      (setq phones-list phones)
      (while (car phones-list)
        (if (string= (bbdb-phone-location (car phones-list))
                     location)
            (setq phones (delete (car phones-list) phones)))
        (setq phones-list (cdr phones-list)))


      (bbdb-record-set-phones record
                              (nconc phones (list phone))))
    (bbdb-change-record record nil)

    ;; update display if record is visible
    (and (get-buffer-window bbdb-buffer-name)
         (bbdb-display-records (list record)))
    nil))

(provide 'bbdb-merge)
