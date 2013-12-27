;;; mldonkey-auth.el --- Part of the Emacs Interface to MLDonkey

;; Copyright (c) 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.0.4

;; mldonkey-auth.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey-auth.el is distributed in the hope that it will be useful,
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
;; mldonkey.el connects to the telnet interface of the mldonkey core
;; and uses a lot of regexps to parse the output.  So it might not work
;; with newer versions of mldonkey.

;;; Documentation:
;;
;; Use M-x mldonkey-auth RET authenticate at the MlDonkey core.  You can
;; set or customize the variables `mldonkey-user' and `mldonkey-passwd'
;; if you don't want to type them again every time.
;;
;; Note: the password is saved as plain text if you customize it or set
;; it in you ~/.emacs!
;;
;; See
;;
;;   <http://mldonkey.berlios.de/modules.php?name=Wiki&pagename=MultiUser>
;;
;; how to set up MlDonkey to require authentication.
;;
;; Example configuration:
;;
;;   ;; if you don't set these variables you will be asked for user and passwd
;;   (setq mldonkey-user "USERNAME")
;;   (setq mldonkey-passwd "PASSWORD")
;;
;;   ;; automatically authenticate after connecting to the core
;;   (add-hook 'mldonkey-motd-hook 'mldonkey-auth)
;;
;;   ;; automatically authenticate at the console
;;   (add-hook 'mldonkey-console-hook 'mldonkey-console-auth)



;;; Code:

(require 'mldonkey)

;;;; custom variables

(defcustom mldonkey-user ""

  "Username used for authentication.

Empty string or nil will ask for a username."

  :group 'mldonkey
  :type 'string)


(defcustom mldonkey-passwd ""

  "Password used for authentication.

Empty string or nil will ask for a password.  Note: this is saved as
text in your custom file!"

  :group 'mldonkey
  :type 'string)


(defun mldonkey-get-username ()

  "Return `mldonkey-user' or ask for a username.

Returns the value of `mldonkey-user' or asks for a username if
`mldonkey-user' is nil or the empty string."

  (if (and mldonkey-user (not (equal mldonkey-user "")))
      mldonkey-user
    (read-from-minibuffer "MlDonkey username: ")))


(defun mldonkey-get-password ()

  "Return `mldonkey-passwd' or ask for a password.

Returns the value of `mldonkey-passwd' or asks for a password if
`mldonkey-passwd' is nil or the empty string."

  (if (and mldonkey-passwd (not (equal mldonkey-passwd "")))
      mldonkey-passwd
    (read-passwd "MlDonkey password: ")))


(defun mldonkey-auth ()

  "Authenticate at the MlDonkey core.

Authenticate yourself at the MlDonkey core.  This function will ask
for a username or password if `mldonkey-user' or `mldonkey-passwd'
are empty or nil."

  (interactive)

  (mldonkey-send-command (concat "auth " (mldonkey-get-username) " "
                                 (mldonkey-get-password) "auth")))

(defun mldonkey-process-auth ()

  "Display the result of the auth command in the echo area.

Displays the results of the auth command in the echo area.  Gives
an error if the authentication failed."

  (mldonkey-strip-empty-lines)
  (mldonkey-strip-regexp "\n")

  (let ((string))
    (setq string (with-current-buffer
                     (get-buffer-create mldonkey-temp-buffer-name)
                   (delete-and-extract-region (point-min) (point-max))))
    (if (equal string "Bad login/password")
        (error (concat "MlDonkey: " string "."))
      (message (concat "MlDonkey: " string ".")))))


(add-to-list 'mldonkey-commands '("auth" mldonkey-process-auth))





;;;; auth at the console

(require 'mldonkey-console)

(defun mldonkey-console-auth ()

  "Function to add to `mldonkey-console-hook'."

  (mldonkey-console-send-command
   (concat "auth " (mldonkey-get-username) " " (mldonkey-get-password))))


(provide 'mldonkey-auth)

;;; mldonkey-auth.el ends here