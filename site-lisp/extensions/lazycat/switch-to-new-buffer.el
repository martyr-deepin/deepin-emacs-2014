;;; switch-to-new-buffer.el --- Switch to a new buffer without prompt for name

;; Copyright © 2008 Kevin Rodgers

;; Author: Kevin Rodgers <kevin.d.rodgers@gmail.com>
;; Created: 2008-09-25
;; Keywords: convenience, files

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Emacs provides 2 commands to edit text in a new buffer: switch-to-buffer
;; and find-file.  The choice between those commands depends on whether
;; the text will ultimately be saved to a file, but the choice is made
;; before the text has been edited.  Each command prompts the user, for
;; a buffer name or a file name, which must be truly new.  And text
;; edited via switch-to-buffer is lost without prompting if emacs or the
;; buffer is killed.
;; 
;; The switch-to-new-buffer command is like switch-to-buffer, but it
;; generates a new buffer name instead of prompting the user.  It sets
;; buffer-offer-save to protect against inadvertant data loss via
;; kill-emacs.  And it adds a buffer-local query function to protect
;; against inadvertant data loss via kill-buffer.
;; 
;; switch-to-new-scratch-buffer and switch-to-new-untitled buffer are
;; convenience commands, for creating new *scratch* buffers (like Emacs)
;; and new "Unitled" buffers (like other text editors).
;; 
;; switch-to-new-buffer is added to the menu bar Buffers menu.

;;; Code:
(defvar switch-to-new-buffer-name nil
  "Default name for buffers created by `switch-to-new-buffer'.")

(defun switch-to-new-buffer ()
  "Switch to a new buffer.
The buffer name is the value of `switch-to-new-buffer-name', or \"*scratch*\"
if that is nil.
See `default-major-mode'."
  (interactive)
  (switch-to-buffer (generate-new-buffer (or switch-to-new-buffer-name "*scratch*")))
  ;; Protect against kill-emacs:
  (setq buffer-offer-save t)
  ;; Protect against kill-buffer:
  (add-hook 'kill-buffer-query-functions
            'switch-to-new-buffer-kill-buffer-query-function
            nil
            t))

(defun switch-to-new-buffer-kill-buffer-query-function ()
  (when (and (buffer-modified-p)
             (yes-or-no-p (format "Buffer %s is modified; save? "
                                  (buffer-name))))
    (save-buffer))
  ;; don't abort kill-buffer:
  t)

(defun switch-to-new-scratch-buffer ()
  "Switch to a new *scratch* buffer.
See `initial-major-mode'."
  (interactive)
  (let ((switch-to-new-buffer-name "*scratch*")
        (default-major-mode initial-major-mode))
    (switch-to-new-buffer)))

(defun switch-to-new-untitled-buffer ()
  "Switch to a new \"Untitled\" buffer.
See `default-major-mode'."
  (interactive)
  (let ((switch-to-new-buffer-name "Untitled"))
    (switch-to-new-buffer)))

(when (featurep 'menu-bar)
  (setq menu-bar-buffers-menu-command-entries
        (append menu-bar-buffers-menu-command-entries
                (list (list 'switch-to-new-buffer
                            'menu-item
                            "Select New Buffer"
                            'switch-to-new-buffer
                            :help "Create a new buffer and select it in the current window")))))

(provide 'switch-to-new-buffer)

;;; switch-to-new-buffer.el ends here
