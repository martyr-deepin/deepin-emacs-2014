;;;; uuid.el --- Universal Unique Identifiers

;; Copyright (C) 2007 Jose E. Marchesi

;; Maintainer: Jose E. Marchesi
;; Keywords: standards

;; $Id: uuid.el,v 1.1 2007/05/29 23:11:41 jemarch Exp $

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;; Commentary

;; This file contain an implementation of the ITU X.667 Recommendation
;; for the generation of Universal Unique Identifiers (also known as
;; Globally Unique Identifiers or GUIDs).
;;
;; Each UUID is a hexadecimal-coded ascii sequence composed by the
;; following fields (separated by the ascii hypen-minus, 45 character,
;; except between the VariantAndClockSeqHigh and ClockSeqLow):
;;
;; - TimeLow                (4 octects => 8 hexadecimal digits)
;; - TimeMid                (2 octects => 4 hexadecimal digits)
;; - VersionAndTimeHigh     (2 octects => 4 hexadecimal digits)
;; - VariantAndClockSeqHigh (1 octect => 2 hexadecimal digits)
;; - ClockSeqLow            (1 octect => 2 hexadecimal digits)
;; - Node                   (6 octects => 12 hexadecimal digits)
;;
;; For example:
;;
;;  00000000-0000-0000-0000-000000000000
;;
;; There are three standarized ways to generate the values of those
;; fields:
;;
;;   - time-based
;;   - random-based
;;   - name-based
;;
;; You can specify the generation semantics to use via the optional
;; `uuid-type' parameter to `uuid-generate'. The default method is the
;; time-based one.
;;
;; Note that, according to the ITU recommendation, uuid generators
;; should generate lower-case letters in hexadecimal encoding. On the
;; other hand, it is recommended for uuid consumers to be
;; case-insensitive regarding alphabetic characters in hex
;; strings. This implementation follows both recommendations.

;;;; Code:

(require 'calc)

(defgroup uuid nil
  "Universal Unique Identifiers"
  :group 'development
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki/UniversalUniqueIdentifiers"))

(defcustom uuid-ifconfig-program
  "/sbin/ifconfig"
  "Location of the `ifconfig' program to determine the MAC
address to use in the time-based method. If it is set to nil,
then a standarized alternative random method is used."
  :group 'uuid)

(defvar uuid-hexoctect-regexp
  "[0-9a-fA-F][0-9a-fA-F]"
  "Regexp that matches the hexadecimal representation of an octect using lower-case letters")

(defvar uuid-time-low-regexp
  (concat uuid-hexoctect-regexp
          uuid-hexoctect-regexp
          uuid-hexoctect-regexp
          uuid-hexoctect-regexp)
  "Regexp that matches the TimeLow field of a uuid")

(defvar uuid-time-mid-regexp
  (concat uuid-hexoctect-regexp
          uuid-hexoctect-regexp)
  "Regexp that matches the TimeMid field of a uuid")

(defvar uuid-version-and-time-high-regexp
  (concat uuid-hexoctect-regexp
          uuid-hexoctect-regexp)
  "Regexp that matches the VersionAndTimeHigh field of a uuid")

(defvar uuid-variant-and-clock-seqhigh-regexp
  uuid-hexoctect-regexp
  "Regexp that matches the VariantAndClockSeqHigh field of a uuid")

(defvar uuid-clock-seq-low-regexp
  uuid-hexoctect-regexp
  "Regexp that matches the ClockSeqLow field of a uuid")

(defvar uuid-node-regexp
  (concat uuid-hexoctect-regexp
          uuid-hexoctect-regexp
          uuid-hexoctect-regexp
          uuid-hexoctect-regexp
          uuid-hexoctect-regexp
          uuid-hexoctect-regexp)
  "Regexp that matches the Node field of a uuid")

(defvar uuid-regexp
  (concat "^"
          uuid-time-low-regexp
          "-"
          uuid-time-mid-regexp
          "-"
          uuid-version-and-time-high-regexp
          "-"
          uuid-variant-and-clock-seqhigh-regexp
          uuid-clock-seq-low-regexp
          "-"
          uuid-node-regexp
          "$")
  "Regexp that matches a uuid hexadecimal-coded value")

(defvar uuid-time-based-version-hex
  "1"
  "Hexadecimal string encoding the time-based version of a uuid")

(defvar uuid-dce-security-version-hex
  "2"
  "Hexadecimal string encoding the reserved DCE security version of a uuid")

(defvar uuid-name-based-md5-version-hex
  "3"
  "Hexadecimal string encoding the name-based version with MD5 hash of a uuid")

(defvar uuid-name-based-sha1-version-hex
  "4"
  "Hexadecimal string encoding the name-based version with SHA-1 hash of a uuid")

(defvar uuid-random-number-based-version-hex
  "5"
  "Hexadecimal string encoding the random-number-based version of a uuid")

(defvar uuid-namespace-dns
  ;; 6BA7B810
  (list #x6BA7 #xB810
        #x9DAD
        #x11D1
        #x80B4 #x00C0 #x4FD4 #x30C8)
  "ITU X.667 recommended namespace for DNS names")

(defvar uuid-namespace-url
  (list #x6BA7 #xB811
        #x9DAD
        #x11D1
        #x80B4 #x00C0 #x4FD4 #x30C8)
  "ITU X.667 recommended namespace for URL names")

(defvar uuid-namespace-oid
  (list #x6BA7 #xB812
        #x9DAD
        #x11D1
        #x80B4 #x00C0 #x4FD4 #x30C8)
  "ITU X.667 recommended namespace for OID names")

(defvar uuid-namespace-x500
  (list #x6BA7 #xB814
        #x9DAD
        #x11D1
        #x80B4 #x00C0 #x4FD4 #x30C8)        
  "ITU X.667 recommended namespace for directory names")

;;;###autoload
(defun uuid-generate (&optional uuid-type namespace name)
  "Generate and return a new universal unique identifier according
with the ITU X.667 Recommendation for the generation of Universal Unique
Identifiers.

If specified, UUID-TYPE identifies the desired uuid type: `time',
`name-md5', `name-sha1' or `random'. It defaults to `time'.

If specified and `name-md5' or `name-sha1' is used, NAMESPACE is
the namespace to use (see `uuid-namespace-XXX' variables).

If specified, NAME is the name for the `name-md5' or `name-sha1'
method."
  (if (not uuid-type)
      (setq uuid-type 'time))
  (cond
   ((equal uuid-type 'time)
    (uuid-generate-time-based))
   ((or (equal uuid-type 'name-md5)
        (equal uuid-type 'name-sha1))
    (when (not (and namespace name))
      (error "You must specify values for both NAMESPACE and NAME"))
    (uuid-generate-name-based uuid-type namespace name))
   ((equal uuid-type 'random)
    (uuid-generate-random-based))
   (t
    (error "Wrong generation algorithm.\
 Valid ones are 'time 'name-md5 'name-sha1 or 'random"))))

(defun uuid-generate-name-based (type namespace name)
  "Generate and return a name-based uuid."
  (let (time-low
        time-mid
        version-and-time-high
        clock-seq-low
        variant-and-clock-seq-high
        node
        hash
        (name-sequence "")
        i)
    ;; Convert the name to a canonical sequence of octets (as defined by the standards or conventions of its
    ;; name space).
    (dotimes (i (length name))
      (setq name-sequence
            (concat name-sequence (format "%.2x" (aref name i)))))
    ;; Compute the 16-octet hash value of the name space identifier
    ;; concatenated with the name, using the hash function specified
    ;; in 14.2 or 14.3. The numbering of the octets in the hash value
    ;; is from 0 to 15, as specified in IETF RFC 1321 (for MD5) and as
    ;; specified in FIPS PUB 180-2 for SHA-1.
    (cond
     ((equal type 'name-sha1) 
      (error "Name-based type method sha1 not implemented"))
     ((equal type 'name-md5)
      (setq hash (md5 (concat (uuid-namespace-to-string namespace)
                              name-sequence))))
     (t
      (error "Wrong name-based type")))

    ;; Set octets 3 through 0 of the "TimeLow" field to octets 3
    ;; through 0 of the hash value.
    (setq time-low 
          (concat (format "%.2x" 0)
                  (substring hash 26)))
    ;; Set octets 1 and 0 of the "TimeMid" field to octets 5 and 4 of
    ;; the hash value.
    (setq time-mid
          (substring hash 22 25))
    ;; Set octets 1 and 0 of the "VersionAndTimeHigh" field to octets
    ;; 7 and 6 of the hash value.

    ;; Overwrite the four most significant bits (bits 15 through 12)
    ;; of the "VersionAndTimeHigh" field with the four-bit version
    ;; number from Table 3 of 12.2 for the hash function that was
    ;; used.

    ;; Set the "VariantAndClockSeqHigh" field to octet 8 of the hash
    ;; value.

    ;; Overwrite the two most significant bits (bits 7 and 6) of the
    ;; "VariantAndClockSeqHigh" field with 1 and 0, respectively.

    ;; Set the "ClockSeqLow" field to octet 9 of the hash value.

    ;; Set octets 5 through 0 of the "Node" field to octets 15 through
    ;; 10 of the hash value.
    (concat time-low "-"
            time-mid "-"
            version-and-time-high "-"
            variant-and-clock-seq-high
            clock-seq-low "-"
            node)))

(defun uuid-generate-random-based ()
  "Generate and return a random-based uuid"
  (let (time-low 
        time-mid
        version-and-time-high
        clock-seq-low
        variant-and-clock-seq-high
        node)
    ;; Set the two most significant bits (bits 7 and 6) of the
    ;; "VariantAndClockSeqHigh" field to 1 and 0, respectively.
    (setq variant-and-clock-seq-high
          (format "%.2x" (logior #x80 (logand #xBF (random (expt 2 8))))))
    ;; Set the four most significant bits (bits 15 through 12) of the
    ;; "VersionAndTimeHigh" field to the four-bit version number
    ;; specified in 12.2.
    (setq version-and-time-high 
          (concat uuid-random-number-based-version-hex
                  (format "%.3x" (random (expt 2 12)))))
    ;; Set all the other bits of the UUID to randomly (or
    ;; pseudo-randomly) generated values.
    (setq time-low 
          (concat (format "%.4x" (random (expt 2 16)))
                  (format "%.4x" (random (expt 2 16)))))
    (setq time-mid 
          (format "%.4x" (random (expt 2 16))))
    (setq clock-seq-low 
          (format "%.2x" (random (expt 2 8))))
    (setq node
          (concat
           (format "%.4x" (random (expt 2 16)))
           (format "%.4x" (random (expt 2 16)))
           (format "%.4x" (random (expt 2 16)))))
    (concat time-low "-"
            time-mid "-"
            version-and-time-high "-"
            variant-and-clock-seq-high
            clock-seq-low "-"
            node)))

(defun uuid-generate-time-based ()
  "Generate and return a time-based uuid"
  ;; Determine the values for the UTC-based Time and the Clock
  ;; Sequence to be used in the UUID, as specified in 12.3 and 12.4.
  (let (time 
        clock-sequence
        time-low time-mid version-and-time-high clock-seq-low
        variant-and-clock-seq-high
        node)
    ;; For the purposes of this algorithm, consider Time to be a
    ;; 60-bit unsigned integer and the Clock Sequence to be a 14-bit
    ;; unsigned integer. 
    (setq time (uuid-generate-time))
    (setq clock-sequence (uuid-generate-clock-sequence))
    ;; Set the "TimeLow" field equal to the least significant 32 bits
    ;; (bits 31 through 0) of Time in the same order of significance.
    (setq time-low 
          (concat 
           (format "%.3x" (logand #x000FF (nth 1 time)))
           (format "%.5x" (nth 2 time))))
    ;; Set the "TimeMid" field equal to bits 47 through 32 from the
    ;; Time in the same order of significance.
    (setq time-mid
          (concat 
           (format "%.2x" (logand #x0007F (nth 0 time)))
           (format "%.2x" (ash (nth 1 time) -12))))
    
    ;; Set the 12 least significant bits (bits 11 through 0) of the
    ;; "VersionAndTimeHigh" field equal to bits 59 through 48 from
    ;; Time in the same order of significance.
    ;; Set the four most significant bits (bits 15 through 12) of the
    ;; "VersionAndTimeHigh" field to the four-bit version number
    ;; specified in 12.2.
    (setq version-and-time-high 
          (concat uuid-time-based-version-hex
                  (format "%.3x" (ash (nth 0 time) -7))))
    ;; Set the "ClockSeqLow" field to the eight least significant bits
    ;; (bits 7 through 0) of the Clock Sequence in the same order of
    ;; significance.
    (setq clock-seq-low 
          (format "%.2x" (logand #x000F clock-sequence)))
    ;; Set the six least significant bits (bits 5 through 0) of the
    ;; "VariantAndClockSeqHigh" field to the six most significant bits
    ;; (bits 13 through 8) of the Clock Sequence in the same order of
    ;; significance.
    ;; Set the two most significant bits (bits 7 and 6) of the
    ;; "VariantAndClockSeqHigh" clock to one and zero, respectively.
    (setq variant-and-clock-seq-high
          (format "%.2x" (logand #x00BF (ash clock-sequence -9))))
    ;; Set the node field to the 48-bit MAC address in the same order
    ;; of significance as the address.
    (let ((mac-address (uuid-get-mac-address)))
      (if mac-address
          (setq node (uuid-format-mac-address mac-address))
        ;; Use a random number
        (setq node 
              (concat
               (format "%.4x" (random (expt 2 16)))
               (format "%.4x" (random (expt 2 16)))
               (format "%.4x" (random (expt 2 16)))))))
    (concat time-low "-"
            time-mid "-"
            version-and-time-high "-"
            variant-and-clock-seq-high
            clock-seq-low "-"
            node)))

(defun uuid-generate-time ()
  "Return the number of 100 nanosecond intervals of UTC since the beginning
of the Gregorian calendar (00:00:00, 15 October 1582).

The returned value is a list:

   (TIME-HIGH TIME-MID TIME-LOW)

with three 20-bits unsigned integers that conform a 60-bit
unsigned integer.

NOTE: we use a resolution of seconds in this code."
  ;; 100 ns intervals offset between Gregorian beginning (00:00:00, 15
  ;; October 1582) and the epoch (00:00:00, 1 January 1970):
  ;; 0x1B21DD213814000

  ;; Operate with 20-bit numbers (GNU Emacs assures integers are 
  ;; at least 29 bits wide and 20/4 = 5)
  (let ((greg-epoch-offset-high #x1B21D)
        (greg-epoch-offset-mid #xD2138)
        (greg-epoch-offset-low #x14000)
        since-epoch-high since-epoch-mid since-epoch-low
        (current-time (current-time)))
    ;; Calculate time since the epoch in seconds
    (setq since-epoch-time-low (+ (nth 1 current-time)
                                  (logand #xF (nth 0 current-time))))
    (setq since-epoch-time-mid (ash (nth 0 current-time) -4))
    (setq since-epoch-time-high (ash (nth 2 current-time) -9))
    ;; TODO: since-epoch-time * 10.000.000
    ;; TODO: Finishme
    (list since-epoch-time-high
          since-epoch-time-mid
          since-epoch-time-low)))

(defun uuid-generate-clock-sequence ()
  "Return a clock sequence number that should be interpreted
as a 14-bit unsigned integer.

NOTE: Since this implementation does not store any state, we
follow the ITU recommendation in using a pseudo-random number
that is _not_ derivated from the Node."
  (random (expt 2 14)))

(defun uuid-format-mac-address (mac-addr)
  "Format MAC-ADDR (a valid MAC address) to a raw hex format"
  (downcase (replace-regexp-in-string ":" "" mac-addr)))

(defun uuid-get-mac-address ()
  "Return a suitable MAC address from a network card in the host computer.
If no MAC address is found, then return nil."
  (when (file-executable-p uuid-ifconfig-program)
    (save-excursion
      (with-temp-buffer 
        (call-process uuid-ifconfig-program nil t nil "-a")
        (goto-char (point-min))
        (when (re-search-forward "HWaddr " nil t)
          (re-search-forward (concat uuid-hexoctect-regexp
                                     ":"
                                     uuid-hexoctect-regexp
                                     ":"
                                     uuid-hexoctect-regexp
                                     ":"
                                     uuid-hexoctect-regexp
                                     ":"
                                     uuid-hexoctect-regexp
                                     ":"
                                     uuid-hexoctect-regexp) nil t)
          (buffer-substring (match-beginning 0)
                            (match-end 0)))))))

(defun uuid-namespace-to-string (namespace)
  "Return the hex string representation of NAMESPACE"
  (concat
   (format "%.4x" (nth 0 namespace))
   (format "%.4x" (nth 1 namespace))
   (format "%.4x" (nth 2 namespace))
   (format "%.4x" (nth 3 namespace))
   (format "%.4x" (nth 4 namespace))
   (format "%.4x" (nth 5 namespace))
   (format "%.4x" (nth 6 namespace))
   (format "%.4x" (nth 7 namespace))))

;;;###autoload
(defun uuidp (uuid)
  "Return t if UUID is a valid uuid"
  (save-match-data
    (when (string-match uuid-regexp uuid)
      t)))

;;;###autoload
(defun uuid-lessp (uuid1 uuid2)
  "Return t if UUID1 is lesser than UUID2."
  (string-lessp uuid1 uuid2))

;;;###autoload
(defun uuid-equal (uuid1 uuid2)
  "Return t if UUID1 and UUID2 are the same uuid."
  (string-equal uuid1 uuid2))

(provide 'uuid)

;;; uuid.el ends here
