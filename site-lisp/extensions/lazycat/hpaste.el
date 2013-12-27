;;; hpaste.el --- Integration with hpaste: http://hpaste.org

;; Filename: hpaste.el
;; Description: Integration with hpaste: http://hpaste.org
;; Author: David House <dmhouse@gmail.com>
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, David House <dmhouse@gmail.com>, all rights reserved.
;; Created: 2008-12-05 22:45:17
;; Version: 1.1
;; Last-Updated: 2008-12-05 22:45:22
;;           By: Andy Stewart
;; URL:
;; Keywords: hpaste, erc
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be requried by this library:
;;
;; `url' `erc'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Integration with hpaste: http://hpaste.org
;;
;; This code is write by David House, and i just improve code and
;; add some code to intergration with ERC.

;;; Installation:
;;
;; Put hpaste.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (setq load-path (append (list (expand-file "~/elisp")) load-path))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'hpaste)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/05
;;      First realead.
;;

;;; Acknowledgements:
;;
;;      David House <dmhouse@gmail.com> for create hpaste.el
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'url)

;;; Code:

(defgroup hpaste nil
  "Integration with the hpaste pastebin"
  :group 'editing)

(defcustom hpaste-server "http://hpaste.org"
  "Base URL for the hpaste server."
  :type '(string)
  :group 'hpaste)
(defcustom hpaste-default-nick nil
  "What to tell the server your nick is. If NIL, then prompt every time."
  :type '(choice (string) (const :tag "Ask every time" nil))
  :group 'hpaste)
(defcustom hpaste-blank-title nil
  "If non-NIL, don't send a title to the server."
  :type '(boolean)
  :group 'hpaste)
(defcustom hpaste-send-as-announce nil
  "If non-NIL, send the paste as an annotation."
  :type '(boolean)
  :group 'hpaste)
(defcustom hpaste-announce 'ask
  "Whether to announce the paste in the #haskell channel on
Freenode. If ALWAYS, then announce every time. If ASK, then
prompt every time. If NEVER, then never announce."
  :type '(choice (const :tag "Always announce" always)
                 (const :tag "Ask each time" ask)
                 (const :tag "Never announce" never))
  :group 'hpaste)

(defvar hpaste-last-paste-id nil
  "Numerical ID of the last paste.")

(defvar hpaste-last-paste-title nil
  "The title of the last paste.")

(defvar hpaste-after-paste-hook nil
  "Run after executing `hpaste-after-paste'.")

(defvar hpaste-create-buffer "*hpaste*"
  "The handle buffer of `hpaste'.")

(defvar hpaste-creation-help
  (concat ";; Enter you paste below, and press C-c C-c to send.\n"
          ";; Press C-c C-d to cancel this paste.\n\n")
  "Paste creation help text.")

(defvar hpaste-window-configuration nil
  "The current window configuartion before do `hpaste-paste-irc'.")

(defvar hpaste-paste-erc-channel nil
  "The channel name that use in `hpaste-paste-erc'.")

(defun hpaste-after-paste (&optional redirect)
  "Callback that runs after a paste is made. Messages the user
and tell them that everything went smoothly, and save the paste
ID for use as a default ID for annotations."
  (message "Paste successful: %s" (cadr redirect))
  (kill-new (format (cadr redirect)))
  (if (eq (car redirect) ':redirect)
      (let (url id)
        (setq url (cadr redirect))
        (string-match "/\\([0-9]*\\)\\(#.*\\)?$" url)
        (setq id (match-string 1 url))
        (if id
            (setq hpaste-last-paste-id id))
        (run-hooks 'hpaste-after-paste-hook))))

(defun hpaste-prompt-for-annotate ()
  "Ask the user whether they want to send the paste as an
annotation, and if so, the ID of the paste to
annotate (defaulting to the last paste made through this
interface)."
  (if hpaste-send-as-announce
      (let* ((prompt
              (if hpaste-last-paste-id
                  (format "Paste to annotate (default %s): "
                          hpaste-last-paste-id)
                "Paste to annotate: "))
             (input (read-from-minibuffer prompt)))
        (if (> (length input) 0) input hpaste-last-paste-id))))

(defun hpaste-paste-region (beg end)
  "Send the region to the hpaste server specified in
`hpaste-server'. Use the nick in `hpaste-default-nick', or prompt
for one if that is NIL. You can still appear as (anonymous) by
just not filling out a nick when prompted (just hit RET). Prompt
for a title, unless `hpaste-blank-title' is non-NIL, in which
case just send a blank title. Pastes will be announced in
#haskell on Freenode according to `hpaste-announce', see the
docstring of that variable for more information.

For more information on hpaste, see http://hpaste.org"
  (interactive "r")
  (let* ((nick (or hpaste-default-nick (read-from-minibuffer "Nick: ")))
         (title (if hpaste-blank-title "" (read-from-minibuffer "Title: ")))
         (annot-id (hpaste-prompt-for-annotate))
         (announce (if (or (eq hpaste-announce 'always)
                           (and (eq hpaste-announce 'ask)
                                (y-or-n-p "Announce paste? ")))
                       "&announce=true"
                     ""))

         (url (concat hpaste-server
                      (if annot-id (concat "/annotate/" annot-id)
                        "/new")))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-mime-accept-string "*/*")
         (url-request-data
          (format "content=%s&nick=%s&title=%s%s&x=0&y=0\r\n"
                  (url-hexify-string (buffer-substring-no-properties beg end))
                  (url-hexify-string nick)
                  (url-hexify-string title)
                  announce)))
    (setq hpaste-last-paste-title title)
    (url-retrieve url 'hpaste-after-paste)))

(defun hpaste-get-paste (id)
  "Fetch the contents of the paste from hpaste into a new buffer."
  (interactive "nPaste #: ")
  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url-mime-accept-string "*/*")
        (url (url-generic-parse-url
              (format "http://hpaste.org/%s/0/plain" id)))
        hpaste-buffer)
    (setq hpaste-buffer (url-retrieve-synchronously url))
    (setq hpaste-last-paste-id id)

    (with-current-buffer hpaste-buffer
      (progn
        (set-visited-file-name (format "hpaste #%s" id))
        (search-forward-regexp "\n\n")
        (delete-region (point-min) (point))
        (set-buffer-modified-p nil)
        (switch-to-buffer hpaste-buffer)
        (if haskell-version
            (haskell-mode)
          (normal-mode))))))

(defun hpaste-paste-buffer ()
  "Like `hpaste-paste-region', but paste the entire buffer instead."
  (interactive)
  (hpaste-paste-region (point-min) (point-max)))

(defun hpaste-paste-dwim ()
  "Paste dwim"
  (interactive)
  (let ((paste-start (point-min))
        (paste-end (point-max)))
    (when mark-active
      (setq paste-start (region-beginning))
      (setq paste-end (region-end))
      (deactivate-mark))
    (hpaste-paste-region paste-start paste-end)))

(defun hpaste-append-paste-buffer ()
  "Append to `hpaste-create-buffer'."
  (interactive)
  (let ((paste-start (point-min))
        (paste-end (point-max))
        paste-content)
    (if (bufferp (get-buffer hpaste-create-buffer))
        (progn
          (when mark-active
            (setq paste-start (region-beginning))
            (setq paste-end (region-end))
            (deactivate-mark))
          (setq paste-content (buffer-substring paste-start paste-end))
          (with-current-buffer hpaste-create-buffer
            (set-buffer-modified-p t)
            (goto-char (point-max))
            (insert paste-content)))
      (message "Please use `hpaste-paste-erc-create' to create paste buffer first."))))

(defun hpaste-paste-quit ()
  "Quit hpaste."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer)
  (when hpaste-window-configuration
    (set-window-configuration hpaste-window-configuration)
    (setq hpaste-window-configuration nil)))

(defun hpaste-paste-send ()
  "Send paste content."
  (interactive)
  ;; Remove `hpaste-creation-help' from buffer
  (with-current-buffer hpaste-create-buffer
    (let (paste-content)
      (goto-char (point-min))
      (setq paste-content (buffer-string))
      (string-match hpaste-creation-help paste-content)
      (setq paste-content (replace-match "" nil nil paste-content 0))
      (erase-buffer)
      (insert paste-content)))
  (hpaste-paste-buffer)
  (hpaste-paste-quit))

(defun hpaste-paste-erc-create ()
  "Create buffer for paste content in speical IRC channel."
  (interactive)
  ;; Remember current window configuration for revert
  (setq hpaste-window-configuration (current-window-configuration))
  ;; Get paste IRC channel
  (if (eq major-mode 'erc-mode)
      (setq hpaste-paste-erc-channel (buffer-name))
    (setq hpaste-paste-erc-channel (read-string "Channel name: ")))
  ;; Create paste buffer
  (switch-to-buffer (get-buffer-create hpaste-create-buffer))
  (set-buffer-modified-p t)
  (erase-buffer)
  (insert hpaste-creation-help)
  (local-set-key (kbd "C-c C-d") 'hpaste-paste-quit)
  (local-set-key (kbd "C-c C-c") 'hpaste-paste-send))

(defun hpaste-paste-erc-show-link ()
  "Show paste link in special IRC channel"
  (when (and hpaste-paste-erc-channel
             (bufferp (get-buffer hpaste-paste-erc-channel)))
    (with-current-buffer hpaste-paste-erc-channel
      (erc-send-action (erc-default-target) (format "pasted \"%s\" at http://hpaste.org/%s" hpaste-last-paste-title hpaste-last-paste-id))
      (setq hpaste-paste-erc-channel nil))))
(add-hook 'hpaste-after-paste-hook 'hpaste-paste-erc-show-link)

(provide 'hpaste)

;;; hpaste.el ends here
