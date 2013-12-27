;;; mldonkey.el --- Emacs Interface to MLDonkey

;; Copyright (c) 2003, 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.0.4

;; mldonkey.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; MLDonkey is a multi-networks peer-to-peer client.
;; See <http://www.nongnu.org/mldonkey/> for details.
;;
;; mldonkey.el connects to the telnet interface of the mldonkey core
;; and uses a lot of regexps to parse the output.  So it might not work
;; with newer versions of mldonkey.

;;; Change log
;; 0.0.4:
;;   * made `mldonkey-temp-buffer-name' hidden buffer
;; 0.0.3b:
;;   * minor bug fixes
;; 0.0.3a:
;;   * changed name of the temp buffer

;;; Documentation:
;;
;; This is the basic module for the MlDonnkey interface.  It only supports
;; connecting to the core and some basic functions.  Read the documentation
;; for the other modules as well.
;;
;; Set `mldonkey-host' and `mldonkey-port' to the hostname and port where
;; you run the MlDonkey core.  You can customize these variables.
;;
;; To open a buffer in mldonkey-mode and connect to the core use
;; M-x mldonkey RET.  To quit the interface just kill the *MlDonkey*
;; buffer.
;;
;; Sometimes the telnet interface seems to hang.  You can reconnect to
;; using M-x mldonkey-reconnect RET.
;;
;; `mldonkey-quit' buries the *MlDonkey* buffer and hides all other
;;  windows with this buffer.  `mldonkey-quit' is bound to the key "q".
;;
;; Example configuration:
;;
;;   (require 'mldonkey)
;;
;;   ;; hostname and port
;;   (setq mldonkey-host "localhost")
;;   (setq mldonkey-port 4000)
;;
;;   ;; the motd looks more nice this way.  make sure the image exists
;;   (setq mldonkey-image "~/.elisp/mldonkey.jpg")

;;; TODO or not TODO
;;
;; - mldonkey.el:
;;
;;    timers
;;
;; - mldonkey-console.el
;;
;;    completion
;;
;; - mldonkey-vdnum.el
;;
;; - mldonkey-preview
;;





;;; Code:

;;;; customization groups

(defgroup mldonkey ()
  "Emacs interface to MlDonkey.")

(defgroup mldonkey-faces ()
  "Faces for the MlDonkey package." :group 'mldonkey)


;;;; custom variables

(defcustom mldonkey-host "localhost"
  "The host which runs the MlDonkey core."
  :group 'mldonkey
  :type 'string)

(defcustom mldonkey-port 4000
  "The port on which the telnet interface is bound."
  :group 'mldonkey
  :type 'integer)

(defcustom mldonkey-image ""
  "Image to display after the motd."
  :group 'mldonkey
  :type 'file)


;;;; faces

(defface mldonkey-motd-face

  '((((type tty) (class color))
     (:foreground "green" :weight bold))
    (((type tty) (class mono))
     (:weight bold))
    (((class color) (background dark))
     (:foreground "chartreuse3" :weight bold))
    (((class color) (background light))
     (:foreground "green3" :weight bold)))

  "Face for displaying the mldonkey motd."

  :group 'mldonkey-faces)





;;;; hooks

(defvar mldonkey-mode-hook nil
  "*Hook called by mldonkey-mode.

Note: The hook is called before openning the connection to the core.")

(defvar mldonkey-open-connection-hook nil
  "*Hook called after connecting to the core.")

(defvar mldonkey-close-connection-hook nil
  "*Hook called after closing the connection to the core.")

(defvar mldonkey-motd-hook nil
  "*Hook called after displaying the motd.")


;;;; regexps

(defconst mldonkey-junk-regexps
  (list "^.*"                         ; at the beginning after connect
        "\\[[0-9;]*m"                 ; ansi colors
        "Use \\? for help[^\n]*\n"
	"MLdonkey command-line:.*"
        "^Welcome on mldonkey command-line\n"
	"^>[^\n]*\n"                    ; input lines
	"\n> ")                         ; end of the output
  "List of regexps which matches junk in the mldonkey-output.")

(defconst mldonkey-end-regexp
  "\n> "
  "Regexp that matches the end of the output")

(defconst mldonkey-need-auth-regexp
  "Command not authorized"
  "Regexp that matches errors due to missing authentication.")


;;;; some other constants

(defconst mldonkey-process-name "mldonkey"
  "Name of the mldonkey process")

(defconst mldonkey-mode-name "MlDonkey"
  "Name of the MlDonkey major mode.")

(defconst mldonkey-buffer-name "*MlDonkey*"
  "Name of the mldonkey buffer.")

(defconst mldonkey-buffer-regexp "^\\*MlDonkey"
  "Regexp used to find the mldonkey buffer in the buffer list.")

(defconst mldonkey-temp-buffer-name " mldonkey-temp"
  "Name of the temporally buffer used for the output of the core.")





;;;; utilities for buffer handling

(defun mldonkey-match-buffer (buffer regexp mode)

  "Check if the name of BUFFER matches REGEXP and BUFFER is in MODE.

Returns t if the name of the buffer BUFFER matches REGEXP
and BUFFER is in the major mode MODE."

  (with-current-buffer buffer
    (and (eq major-mode mode)
         (string-match regexp (buffer-name buffer)))))


(defun mldonkey-find-buffer (regexp mode)

  "Find a buffer which name matches REGEXP and which is in MODE.

Returns the first buffer in the buffer list which name matches
REGEXP and which is in major-mode MODE."

  (let ((buffer))
    (mapc (lambda (buf)
            (unless buffer ;; if buffer is non nil we already found it
              (when (mldonkey-match-buffer buf regexp mode)
                (setq buffer buf))))
          (buffer-list))
    buffer))


(defun mldonkey-get-mldonkey-buffer ()

  "Return the mldonkey buffer.

Returns the mldonkey buffer or nil if there is no mldonkey buffer."

  (mldonkey-find-buffer mldonkey-buffer-regexp 'mldonkey-mode))


(defun mldonkey-get-temp-buffer ()

  "Return the temporally mldonkey buffer.

Returns the tmeporally mldonkey buffer or nil if theres is no
temporally mldonkey buffer"

  (mldonkey-find-buffer mldonkey-temp-buffer-name 'fundamental-mode))


(defun mldonkey-erase-buffers ()

  "Erase the mldonkey buffer and the temporally buffer."

  (let ((inhibt-read-only t) (buffer (mldonkey-get-mldonkey-buffer)))
    (with-current-buffer buffer
      (erase-buffer))
    (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
      (erase-buffer))))





;;;; networking functions

(defvar mldonkey-process nil
  "The process object for the network connection to the core.")

(defvar mldonkey-is-waiting nil
  "Non nil if output from the MlDonkey core is expected.")

(defvar mldonkey-last-command nil
  "Name of the last command send to the MlDonkey core.")

(defvar mldonkey-reconnect-timer nil
  "Timer used for automatic reconnecting if the process exits abnormally.")

(defun mldonkey-is-connected ()

  "Check if there is an open connection to the MlDonkey core.

Returns t if a connection to the MlDonkey core is established,
nil otherwise."

  (if (not mldonkey-process) nil
    (equal (process-status mldonkey-process) 'open)))


(defun mldonkey-open-connection ()

  "Open a connection to the MlDonkey core.

Opens a connection to the MlDonkey core.  Gives an error if there
is already an open connection.  Erases the mldonkey buffer and the
temporally buffer."

  (if (mldonkey-is-connected)
      (error "We are already connected to the MlDonkey core.")

    ;; we can cancel the timer without errors even if it's inactive
    (when mldonkey-reconnect-timer
      (cancel-timer mldonkey-reconnect-timer)
      (setq mldonkey-reconnect-timer nil))
    (message "Opening connection to the MlDonkey core.")
    (setq mldonkey-process (open-network-stream
                            mldonkey-process-name mldonkey-buffer-name
                            mldonkey-host mldonkey-port))
    (set-process-filter mldonkey-process 'mldonkey-filter-output)
    (set-process-sentinel mldonkey-process 'mldonkey-process-sentinel)
    (run-hooks 'mldonkey-open-connection-hook)
    (let ((inhibit-read-only t)) (mldonkey-erase-buffers))
    (setq mldonkey-last-command "motd")
    (setq mldonkey-is-waiting t)))


(defun mldonkey-close-connection ()

  "Close the connection to the MlDonkey core.

Closes the connection to the core.  Gives an error if there is no
open connection."

  (if (not (mldonkey-is-connected))
      (error "We are not connected to the MlDonkey core.")
    (message "Closing connection to the MlDonkey core.")
    (process-send-string mldonkey-process "q\n")
    (delete-process mldonkey-process)
    (run-hooks 'mldonkey-close-connection-hook)))


(defun mldonkey-kill-connection ()

  "Close the connection to the MlDonkey core by deleting the process.

Only deletes the process and sends no \"q\\n\".  Use this function if
the interface hangs."

  (if (not (mldonkey-is-connected))
      (error "We are not connected to the MlDonkey core.")
    (message "Killing connection to the MlDonkey core.")
    (delete-process mldonkey-process)
    (run-hooks 'mldonkey-close-connection-hook)))


(defun mldonkey-reconnect ()

  "Reconnect to the MlDonkey core.

Sometimes the telnet interface seems to hang.  Use this function to
reconnect to the core."

  (interactive)

  ;; just connect if we are not connected to the core
  (if (not (mldonkey-is-connected))
      (mldonkey-open-connection)
    ;; if we are waiting for output the interface seems to hang
    (if mldonkey-is-waiting
        (mldonkey-kill-connection)
      (mldonkey-close-connection))
    (mldonkey-open-connection)))


(defun mldonkey-send-command (string command)

  "Send STRING to the core and set `mldonkey-last-command' to COMMAND.

Gives an error if there is no open connection to the core or the last
command isn't finished yet."

  (if (not (mldonkey-is-connected))
      (error "We are not connected to the MlDonkey core.")
    (if mldonkey-is-waiting
        (error "MlDonkey: Last command not finished yet.")
      (process-send-string mldonkey-process (concat string "\n"))
      (setq mldonkey-last-command command))))


(defun mldonkey-process-sentinel (process event)

  (if (string-match "exited abnormally with code" event)
      ;; reconnect in 5 minutes
      (setq mldonkey-reconnect-timer
            (run-with-timer 300 nil 'mldonkey-open-connection))
    ;; FIXME: i don't know what else might cause the process to exit
    (aset event (1- (length event)) ?.)
    (message (format "Process: %s had the event %s" process event))))





;;;; parsing the output from the mldonkey-core

(defun mldonkey-filter-output (connection string)

  "Filter the CONNECTION output STRING from the mldonkey process.

The filter function for the network stream process to the core.
Strips some junk and writes the output to a temporally buffer.

Gives an error if you need to authenticate."

  (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
    (goto-char (point-max))
    (insert string))

  ;; is the output finished?
  (when (string-match mldonkey-end-regexp string)
    (mapc 'mldonkey-strip-regexp mldonkey-junk-regexps)
    ;; set it back to nil here in case any command handler issues a new
    ;; command (or the user via hooks).  The command handler must make
    ;; sure that the mldonkey-temp-buffer is empty again.
    (setq mldonkey-is-waiting nil)

    ;; check if authentication is required
    (save-excursion
      (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
        (goto-char (point-min))
        (when (re-search-forward mldonkey-need-auth-regexp nil t)
          (erase-buffer)
          (error "MlDonkey: Authentication required."))))

    ;; call the process function for the last command
    (with-current-buffer (mldonkey-get-mldonkey-buffer)
      (let ((inhibt-read-only t)
            (command (assoc mldonkey-last-command mldonkey-commands)))
        (if command
            (funcall (cadr command))
          (error (concat "MlDonkey: No such command: "
                         mldonkey-last-command)))))))


(defun mldonkey-strip-regexp (regexp)

  "Remove matches of REGEXP from the temporally mldonkey buffer."

  (save-excursion
    (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match "")))))


(defun mldonkey-strip-empty-lines ()

  "Removes empty lines from the temporally mldonkey buffer."

  (save-excursion
    (with-current-buffer (get-buffer-create mldonkey-temp-buffer-name)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+\n" nil t)
        (replace-match "")))))


(defun mldonkey-replace-all-matches (regexp replace string)

  "Replace all matches of REGEXP with REPLACE in string."

  (let ((start-index 0))
    (while (setq start-index (string-match regexp string start-index))
      (setq string (replace-match replace t t string))))
  string)





;;;; command interfaces

;; Use this as a sample implementation of a command.  Implement your own
;; commands by writing a process function and adding an entry to
;; `mldonkey-commands'

(defvar mldonkey-commands '()

  "Commands known to the MlDonkey interface.

An alist which maps the name of a command to a function which processes
the output of the the command.")


(defun mldonkey-process-motd ()

  "Use the output of the motd and present it in the mldonkey buffer."

  (mldonkey-strip-empty-lines)

  (let ((inhibit-read-only t))
    ;; insert the text
    (let ((string))
      (insert "\n")
      (setq string (with-current-buffer
                       (get-buffer-create mldonkey-temp-buffer-name)
                     (delete-and-extract-region (point-min) (point-max))))
      (mldonkey-insert-propertized string '(face mldonkey-motd-face))
      (insert "\n\n"))

    ;; insert the image
    (when (and (display-graphic-p) mldonkey-image (not (equal mldonkey-image ""))
               (file-readable-p mldonkey-image))
      (let ((image (create-image mldonkey-image)) (n 0) (move))
        (when image
          (setq move (/ (- (frame-width) (car (image-size image nil))) 2))
          (if (< move 0) (setq move 0))
          (while (< n move)
            (insert " ")
            (setq n (1+ n)))
          (insert-image image)
          (goto-char (point-min)))))

    (run-hooks 'mldonkey-motd-hook)))

(add-to-list 'mldonkey-commands '("motd" mldonkey-process-motd))





;;;; key map

(defvar mldonkey-mode-map nil
  "Local keymap for MlDonkey mode buffers.")

(unless mldonkey-mode-map
  (setq mldonkey-mode-map (make-sparse-keymap)))

(define-key mldonkey-mode-map "q" 'mldonkey-quit)
(define-key mldonkey-mode-map " " 'scroll-up)
(define-key mldonkey-mode-map "\d" 'scroll-down)




;;;; major mode definition

;;;###autoload
(defun mldonkey ()

  "Run the MlDonkey interface.

Creates a new buffer, puts it in `mldonkey-mode' and connects to
the core.  If there is already a MlDonkey buffer it just switches
to this buffer."

  (interactive)

  ;; check if there is already a *MlDonkey* buffer
  (let ((buffer))
    (if (setq buffer (mldonkey-get-mldonkey-buffer))
        (switch-to-buffer buffer)
      ;; if not create one an put it in mldonkey-mode
      (switch-to-buffer (get-buffer-create mldonkey-buffer-name))
      (mldonkey-mode)
      ;; connect
      (unless (mldonkey-is-connected)
        (mldonkey-open-connection)))))


(defun mldonkey-mode ()

  "Put the current buffer in the MlDonkey major mode.

Use `mldonkey' to create a buffer in mldonkey-mode.
\\{mldonkey-mode-map}"

  (kill-all-local-variables)
  (setq major-mode 'mldonkey-mode)
  (setq mode-name mldonkey-mode-name)
  (use-local-map mldonkey-mode-map)

  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook 'mldonkey-kill-buffer-function)
  (run-hooks 'mldonkey-mode-hook))


(defun mldonkey-kill-buffer-function ()

  "Clean up when the mldonkey buffer gets killed.

Closes the connection to the core and removes the temporally buffer."

  (let ((buffer (mldonkey-get-mldonkey-buffer))
        (temp-buffer (mldonkey-get-temp-buffer)))
    (when (equal (current-buffer) buffer)
      (if temp-buffer (kill-buffer temp-buffer))
      (when (mldonkey-is-connected) (mldonkey-close-connection)))))


(defun mldonkey-quit ()

  "Burry the MlDonkey buffer."

  (interactive)

  (when (equal major-mode 'mldonkey-mode)
    (bury-buffer (current-buffer))
    (delete-windows-on (current-buffer))))





;;;; some functions that might be useful for other modules

(defun mldonkey-insert-propertized (string properties)

  "Insert STRING with the given PROPERTIES in the current buffer."

  (when (> (length string) 0)
    (let ((start (point)))
      (insert string)
      (add-text-properties start (point) properties))))


;; c&p from the emacs cvs sources

(unless (fboundp 'number-sequence)
  (defun number-sequence (from &optional to inc)
    (if (or (not to) (= from to))
	(list from)
      (or inc (setq inc 1))
      (when (zerop inc) (error "The increment can not be zero"))
      (let (seq (n 0) (next from))
	(if (> inc 0)
	    (while (<= next to)
	      (setq seq (cons next seq)
		    n (1+ n)
		    next (+ from (* n inc))))
	  (while (>= next to)
	    (setq seq (cons next seq)
		  n (1+ n)
		  next (+ from (* n inc)))))
	(nreverse seq)))))


;; c&p from the emacs cvs sources

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))





;;;; load all other modules

(provide 'mldonkey)

(require 'mldonkey-console)
(require 'mldonkey-auth)
(require 'mldonkey-vd)
(require 'mldonkey-vd-sort)
(require 'mldonkey-commands)
(require 'mldonkey-header-line)




;;; mldonkey.el ends here