;;; bbdb-whois.el -- Big Brother gets a little help from Big Brother
;;; This file is part of the Insidious Big Brother Database (aka BBDB).
;;;
;;; Copyright (C) 1992, 1993 Roland McGrath
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@gnu.ai.mit.edu) or
;;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to bbdb@waider.ie

(require 'bbdb-com)

(defmacro bbdb-add-to-field (record field text)
  (let ((get (intern (concat "bbdb-record-" (symbol-name field))))
    (set (intern (concat "bbdb-record-set-" (symbol-name field)))))
    (` (let ((old ((, get) (, record)))
         (text (, text)))
     (or (member text old)
         ((, set) (, record) (nconc old (list text))))))))

(defcustom bbdb-whois-server (or (and (boundp 'whois-server) whois-server)
                                 "whois.geektools.com")
  "*Server for \\[bbdb-whois] lookups."
  :group 'bbdb-utilities
  :type 'string)

(defvar bbdb-whois-name nil
  "Used to store the name during a whois call.")
(make-variable-buffer-local 'bbdb-whois-name)
(defvar bbdb-whois-record nil
  "Used to store the record during a whois call.")
(make-variable-buffer-local 'bbdb-whois-record)

;;; main entry point. it'd be nice if we could bbdb-whois an arbitrary
;;; name and make a record from that directly.

;;;###autoload
(defun bbdb-whois (the-record &optional server)
  (interactive (list (bbdb-get-record "BBDB Whois: ")
             (and current-prefix-arg
              (read-string "Query whois server: "
                       bbdb-whois-server))))
  (or server (setq server bbdb-whois-server))
  (if (or (bbdb-record-lastname the-record) (bbdb-record-firstname the-record))
      ;; XXX we seem to get called with a vector of nils.
      (save-excursion
    (set-buffer (generate-new-buffer " *bbdb-whois*"))
    (set bbdb-whois-record the-record)
    (set bbdb-whois-name
         (if (bbdb-record-getprop the-record 'nic)
             (concat "!" (bbdb-record-getprop the-record 'nic))
           (concat (bbdb-record-lastname the-record) ", "
                   (bbdb-record-firstname the-record))))
    (let ((proc (open-network-stream "whois" (current-buffer) server 43)))
      (set-process-sentinel proc 'bbdb-whois-sentinel)
      (process-send-string proc (concat bbdb-whois-name "\r\n"))))))

;;; This function parses the results from the server.
(defun bbdb-whois-sentinel (proc status)
  (save-excursion
    (let (rec)
      (set-buffer (process-buffer proc))
      (setq rec bbdb-whois-record)
      (goto-char 1)

      ;; check for multiple replies
      ;; should maybe present a menu/completion buffer of multiples and do a
      ;; refetch.
      (if (not (re-search-forward "Record last updated" (point-max) t))
          (if (re-search-forward "No match" (point-max) t)
              (message "Can not find a whois record for `%s'" bbdb-whois-name)
            (if (re-search-forward "Access Limit Exceeded" (point-max) t)
                (message "Per-day access limit to %s exceeded."
                         bbdb-whois-server) ;; bah!
              (message "%s is ambiguous to whois; try a different name"
                       bbdb-whois-name)))

        ;; clean up & parse buffer, otherwise.
        (replace-string "\r\n" "\n")
        (goto-char 1)
        (if (re-search-forward
             (concat (if (string-match "^!" bbdb-whois-name)
                         (concat "(\\("
                                 (regexp-quote (substring bbdb-whois-name 1))
                 "\\))")
               (concat (regexp-quote bbdb-whois-name)
                   ".*(\\([A-Z0-9]+\\))"))
             "\\s *\\(\\S +@\\S +\\)?$")
         nil t)
        (let ((net (if (match-beginning 2)
               (downcase (buffer-substring (match-beginning 2)
                               (match-end 2)))))
          (nic (buffer-substring (match-beginning 1) (match-end 1)))
          (lines nil))
          (if net
          (bbdb-add-to-field rec net net))
          (bbdb-record-putprop rec 'nic nic)

          ;; Snarf company.
          ;; not all nic records have companies, though.
          (forward-line 1)
          (back-to-indentation)
          (let ((company (buffer-substring (point) (progn (end-of-line)
                                  (point))))
            (old (bbdb-record-company rec)))
        (cond ((not old)
               (bbdb-record-set-company rec company))
              ((string= old company)
               nil)
              (t
               (bbdb-record-putprop rec 'nic-organization company))))

          ;; Read the address info into LINES.
          (while (progn (forward-line 1)
                (not (looking-at "^$")))
            (back-to-indentation)
            (setq lines (cons (buffer-substring (point)
                                                (progn (end-of-line)
                                                       (point)))
                              lines)))

          ;; Snarf phone number.
          ;; phone, fax are presented, it seems, as
          ;; +country area prefix number +country area prefix number
          ;; we can look for the " +" and split there, I guess.
          (if (car lines)
              (let ((phones (car lines))
                    (n 1)
                    phone-numbers)
                (while (string-match "^\\(.+\\) \\+" phones)
                  (setq phone-numbers
                        (append phone-numbers
                                (list (substring phones 0 (match-end 1))))
                        phones (substring phones (+ 1 (match-end 1)))))
                (setq phone-numbers (append phone-numbers
                                            (list phones)))

                ;; now add each member of the list to the bbdb record
                ;; it'd be nice if we could be smarter about this.
                (mapcar (function
                         (lambda(p)
                           (if (not (bbdb-find-phone
                                     p (bbdb-record-phones rec)))
                               (let ((p-n
                                      (vector (format "nic-phone-%d" n) p)))
                                 (bbdb-add-to-field rec phones p-n)
                                 (setq n (+ 1 n))))))
                        phone-numbers)

                ;; throw away phones line from what we've snarfed
                (setq lines (cdr lines))))

          ;; Snarf address.
          (if (car lines)
              (let ((addr (make-vector bbdb-address-length nil))
                    (city "")
                    (state "")
                    (zip "")
                    (country ""))

                ;; extract country
                (if (string-match "^[A-Z][A-Z]$" (car lines))
                    (setq country (car lines) ;; could convert from ISO...
                          lines (cdr lines)))

                ;; extract city, state, zip
                ;; it would be nice if this could all use bbdb-snarf.
                ;; or if NICs would hand out something machine
                ;; readable, like <shudder> XML.
                ;;
                ;; note the zipcode check at the end of the regexp
                ;; isn't really a zipcode check, because we don't do
                ;; zipcode checks any more.
                (if (string-match
                     "\\([^,]+\\),\\s *\\(\\S +\\)\\s *\\(.+\\)"
                     (car lines))
                    (setq city (substring (car lines)
                                          (match-beginning 1)
                                          (match-end 1))
                          state (substring (car lines)
                                           (match-beginning 2)
                                           (match-end 2))
                          zip (substring (car lines)
                                         (match-beginning 3)
                                         (match-end 3))
                          lines (cdr lines))
                  ;; otherwise we just stuff everything into the
                  ;; streets list and let the user clean it up. This
                  ;; would be nice to do heuristically, if I knew
                  ;; enough about variable address formats.
                  ;; (bbdb-snarf-grok-address (ADDR)) would be neat.
                  )

                (bbdb-address-set-location addr "nic-address")
                (bbdb-address-set-city addr (or city ""))
                (bbdb-address-set-state addr (or state ""))
                (bbdb-address-set-zip addr (or zip ""))
                (bbdb-address-set-country addr (or country ""))
                (setq lines (nreverse lines))
                (bbdb-address-set-streets addr lines)

                ;; should probably overwrite existing nic-address field.
                (bbdb-add-to-field rec addresses addr)))

          ;; Snarf any random notes.
          (setq lines nil)
          (while (progn
                   (forward-line 1)
                   (back-to-indentation)
                   (not (looking-at
                         "$\\|Record last updated on")))
            (if (looking-at "Alternate mailbox: \\(\\S +\\)$")
                (bbdb-add-to-field rec net
                                   (buffer-substring (match-beginning 1)
                                                     (match-end 1)))
              (setq lines (cons (buffer-substring (point)
                                                  (progn (end-of-line)
                                                         (point)))
                                lines))))
          (if lines
              (bbdb-record-putprop rec 'nic-notes
                                   (mapconcat 'identity
                                              (nreverse lines)
                                              "\n")))

          ;; Snarf the last-update date.
          (if (re-search-forward "Record last updated on \\(\\S *\\)\\."
                                 nil t)
              (bbdb-record-putprop rec 'nic-updated
                                   (buffer-substring (match-beginning 1)
                                                     (match-end 1))))

          (save-excursion
            (set-buffer bbdb-buffer-name)
            (bbdb-redisplay-one-record rec)))
        (message "No whois information for %s" bbdb-whois-name)))
      (delete-process proc)
      (kill-buffer (current-buffer)))))

(defun bbdb-find-phone (string record)
  "Return the vector entry if STRING is a phone number listed in RECORD."
  (let ((phone nil)
    (done nil))
    (while (and record (not done))
      (setq phone (car record))
      (if (string= string (bbdb-phone-string phone))
      (setq done phone))
      (setq record (cdr record)))
    done))

(provide 'bbdb-whois)
