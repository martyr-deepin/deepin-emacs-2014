;;; mldonkey-commands.el --- Part of the Emacs Interface to MLDonkey

;; Copyright (c) 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.0.4

;; mldonkey-commands.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey-commands.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file is part if the mldonkey.el package.
;;
;; MLDonkey is a multi-networks peer-to-peer client.
;; See <http://www.nongnu.org/mldonkey/> for details.
;;
;; mldonkey.el connects to the telnet interface of the MlDonkey core
;; and uses a lot of regexps to parse the output.  So it might not work
;; with future versions of MlDonkey.

;;; Change log
;; 0.0.4:
;;   * fixed bug in `mldonkey-commands-get-file'

;;; Documentation:
;;
;;  This module implements some small but useful commands:
;;
;;  `mldonkey-bw-stats' (bound to "b") shows the bandwidth stats in the
;;  echo area.
;;



;;; Code:

(require 'mldonkey)
(require 'mldonkey-vd)
(require 'mldonkey-console)




;;;; simple commands that just give one line of text as the output

(defconst mldonkey-simple-command-regexp "^\\(.*[^ \t]+[^\n]\\)\n"
  "Matches the one line of a simple command.")


(defvar mldonkey-reshare-hook nil
  "*Hook called after resharing the files.")

(defvar mldonkey-commit-hook nil
  "*Hook called after committing the downloads.")

(defvar mldonkey-recover-temp-hook nil
  "*Hook called after recovering temp files.")


(defun mldonkey-process-simple-command ()

  "Parse the output of a simple command echo the result."

    (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
      (goto-char (point-min))

      ;; get the whole string
      (let ((output))
        (when (re-search-forward mldonkey-bw-stats-line-regexp nil t)
          (setq output (buffer-substring (match-beginning 1) (match-end 1)))
          (erase-buffer)
          (message output)))))


(defun mldonkey-reshare ()

  "Send the \"reshare\" command to the MlDonkey core."

  (interactive)

  (mldonkey-send-command "reshare" "reshare"))


(defun mldonkey-commit ()

  "Send the \"commit\" command to the MlDonkey core."

  (interactive)

  (mldonkey-send-command "commit" "commit"))


(defun mldonkey-recover-temp ()

  "Send the \"recover_temp\" command to the MlDonkey core."

  (interactive)

  (mldonkey-send-command "recover_temp" "recover_temp"))


(defun mldonkey-process-reshare ()

  "Parse the output of the \"reshare\" command and echo the result."

  (mldonkey-process-simple-command)
  (run-hooks 'mldonkey-reshare-hook))


(defun mldonkey-process-commit ()

  "Parse the output of the \"commit\" command and echo the result."

  (mldonkey-process-simple-command)
  (run-hooks 'mldonkey-commit-hook))

(defun mldonkey-process-recover-temp ()

  "Parse the output of the \"recover_temp\" command and echo the result."

  (mldonkey-process-simple-command)
  (run-hooks 'mldonkey-recover-temp-hook))


(add-to-list 'mldonkey-commands '("reshare" mldonkey-process-reshare))

(define-key mldonkey-mode-map "\C-c\C-r" 'mldonkey-reshare)

(add-to-list 'mldonkey-commands '("commit" mldonkey-process-commit))

(define-key mldonkey-mode-map "\C-c\C-c" 'mldonkey-commit)

(add-to-list 'mldonkey-commands '("recover_temp" mldonkey-process-recover-temp))

(define-key mldonkey-mode-map "\C-c\C-t" 'mldonkey-recover-temp)





;;;; bw_stats

(defconst mldonkey-bw-stats-line-regexp "^\\(.*[^ \t]+[^\n]\\)\n"
  "Matches the one line output of the bw_stats command.")

(defconst mldonkey-bw-stats-rates-regexp
  (concat
   "\\([0-9\\.]+[ \t]+[a-zA-Z]+/s\\)"
   ".*?"
   "\\([0-9\\.]+[ \t]+[a-zA-Z]+/s\\)")
  "Matches the up- and download rates of the \"bw_stats\" output.")

(defvar mldonkey-bw-stats-down nil
  "Current downloading rate.")

(defvar mldonkey-bw-stats-up nil
  "Current uploading rate.")

(defvar mldonkey-bw-stats-no-echo nil
  "Non-nil means not to show the bandwidth stats in the echo area.")

(defvar mldonkey-bw-stats-hook nil
  "*Hook called after we received the bandwidth stats.")


(defun mldonkey-bw-stats-init ()

  "Initialize the variables to nil."

  (setq mldonkey-bw-stats-down nil)
  (setq mldonkey-bw-stats-up nil)
  (setq mldonkey-bw-stats-no-echo nil))


(add-hook 'mldoney-open-connection-hook 'mldonkey-bw-stats-init)


(defun mldonkey-bw-stats (&optional no-echo)

  "Get the bandwidth stats.  If NO-ECHO is non nil don't show them."

  (interactive "P")

  (if no-echo
      (setq mldonkey-bw-stats-no-echo t))
  (mldonkey-send-command "bw_stats" "bw_stats"))


(defun mldonkey-process-bw-stats ()

  "Parse the output of the \"bw_stats\" command.

Set the `mldonkey-bw-stats-up' and `mldonkey-bw-stats-down' variables.
Unless mldonkey-bw-stats is non nil echo the output from the core in the
echo area."

  (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
    (goto-char (point-min))

    ;; get the whole string
    (let ((output))
      (when (re-search-forward mldonkey-bw-stats-line-regexp nil t)
        (setq output (buffer-substring (match-beginning 1) (match-end 1)))
        ;; get up- and download rate
        (when (string-match mldonkey-bw-stats-rates-regexp output)
          (setq mldonkey-bw-stats-down (match-string 1 output))
          (setq mldonkey-bw-stats-up (match-string 2 output))))
      (erase-buffer)
      (unless mldonkey-bw-stats-no-echo
        (message output)))
    (run-hooks 'mldonkey-bw-stats-hook))
  (setq mldonkey-bw-stats-no-echo nil))


(add-to-list 'mldonkey-commands '("bw_stats" mldonkey-process-bw-stats))

(define-key mldonkey-mode-map "b" 'mldonkey-bw-stats)




;;;; commands working on a download

(defvar mldonkey-pause-hook nil
  "*Hook called after pausing a download.")

(defvar mldonkey-resume-hook nil
  "*Hook called after resuming a download.")


(defun mldonkey-commands-is-download (number)

  "Return non nil if there is a download with number NUMBER."

  (let ((result nil))
    (dolist (dl mldonkey-vd-downloading-list result)
      (setq result (or result (eq (string-to-number (aref dl 1)) number))))))


(defun mldonkey-commands-get-file ()

  "Returns the number of the download under the cursor.

If there is no running download under the cursor, return nil."

  (if (not (eq (current-buffer) (mldonkey-get-mldonkey-buffer)))
      nil
    (let* ((line (line-number-at-pos)) (num (- line 4)))
      (if mldonkey-vd-num-downloading
          (if (or (< num 0) (>= num mldonkey-vd-num-downloading))
              nil
            (string-to-number (aref (nth num mldonkey-vd-downloading-list) 1)))
        nil))))


(defun mldonkey-commands-ask-dl-number ()

  "Ask for a download number."

  (let ((arg "") (number (mldonkey-commands-get-file)))
    (if number
        (setq arg (number-to-string number)))
    (setq arg (read-from-minibuffer
               "MLDonkey: number of download: " arg))
    (if (setq number (string-to-number arg))
        (if (mldonkey-commands-is-download number)
            number
          (error "No such download."))
      (error "No such download."))))


(defun mldonkey-commands-with-number (command &optional number)

  "Send a command that requires a download as the argument."

  (when (or (not number) (listp number))
    ;; the user used C-u without a number or no argument
    (setq number (mldonkey-commands-ask-dl-number)))
  (unless (mldonkey-commands-is-download number)
    (error "No such download."))

  (mldonkey-send-command
   (concat command " " (number-to-string number)) command))


(defun mldonkey-pause (&optional number)

  "Pause the download with number NUMBER."

  (interactive)

  (mldonkey-commands-with-number "pause" number))


(defun mldonkey-resume (&optional number)

  "Resume the download with number NUMBER."

  (interactive)

  (mldonkey-commands-with-number "resume" number))


(defun mldonkey-process-pause ()

  "Parse the output of the \"pause\" command and echo the result."

  (mldonkey-process-simple-command)
  (run-hooks 'mldonkey-pause-hook))


(defun mldonkey-process-resume ()

  "Parse the output of the \"resume\" command and echo the result."

  (mldonkey-process-simple-command)
  (run-hooks 'mldonkey-resume-hook))


(add-to-list 'mldonkey-commands '("pause" mldonkey-process-pause))

(define-key mldonkey-mode-map "P" 'mldonkey-pause)

(add-to-list 'mldonkey-commands '("resume" mldonkey-process-resume))

(define-key mldonkey-mode-map "R" 'mldonkey-resume)


;;;; commands send directly to the console

(defun mldonkey-cancel (&optional number)

  "Cancel the download under the cursor or with the given NUMBER."

  (interactive)

  (when (or (not number) (listp number))
    ;; the user used C-u without a number or no argument
    (setq number (mldonkey-commands-ask-dl-number)))
  (unless (mldonkey-commands-is-download number)
    (error "No such download."))

  (mldonkey-console-send-command (concat "cancel " (number-to-string number)) t))


(defun mldonkey-vd-num (&optional number)

  "View the detail of the download with number NUMBER.

If number is nil prompt for a number."

  (when (or (not number) (listp number))
    ;; the user used C-u without a number or no argument
    (setq number (mldonkey-commands-ask-dl-number)))
  (unless (mldonkey-commands-is-download number)
    (error "No such download."))

  (mldonkey-console-send-command (concat "vd " (number-to-string number)) t))


(defun mldonkey-vd-num-maybe ()

  "Run the `mldonkey-vd-num' function if the cursor is on a download."

  (interactive)

  (let ((number (mldonkey-commands-get-file)))
    (when number
      (mldonkey-console-send-command
       (concat "vd " (number-to-string number)) t))))


(define-key mldonkey-mode-map "C" 'mldonkey-cancel)

(define-key mldonkey-mode-map "\C-m" 'mldonkey-vd-num-maybe)

(provide 'mldonkey-commands)

;;; mldonkey-commands.el ends here.