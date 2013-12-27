;;; pick-backup.el --- easy access to versioned backup files
;;
;; Copyright (C) 2007 Nikolaj Schumacher <bugs * nschum , de>
;;
;;; License ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add the following to your .emacs file:
;; (require 'pick-backup)
;;
;;; Usage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use `pick-backup-and-ediff', `pick-backup-and-diff',
;; `pick-backup-and-revert', and `pick-backup-and-view'.
;;
;; Use `pick-backup-file' in your own code.
;;
;;; Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2007-04-26 (0.8)
;;     Initial release.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'diff)
(require 'ediff)

;;; Customizable Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup pick-backup nil
  "Easy access your backup files."
  :group 'diff
  :group 'backup)

(defcustom pick-backup-time-format "%Y-%m-%d %H:%M"
  "The date format displayed when picking a backup file."
  :type 'string
  :group 'pick-backup)

;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun pick-backup-and-ediff (file)
  "Diff FILE with one of its backups."
  (interactive (list (buffer-file-name)))
  (unless file (setq file (buffer-file-name)))
  (ediff-files (pick-backup-file file) file))

;;;###autoload
(defun pick-backup-and-diff (file switches)
  "Run Ediff on FILE and one of its backups."
  (interactive (list (buffer-file-name) (diff-switches)))
  (unless file (setq file (buffer-file-name)))
  (diff (pick-backup-file file) file switches))

;;;###autoload
(defun pick-backup-and-revert ()
  "Replace FILE with one of its backups."
  (interactive)
  (let ((backup (pick-backup-file (buffer-file-name))))
    (erase-buffer)
    (insert-file-contents backup)))

;;;###autoload
(defun pick-backup-and-view ()
  "View one of FILE's backups."
  (interactive)
  (view-file (pick-backup-file (buffer-file-name))))

;;; Internal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pick-backup-calling-buffer nil
  "The buffer from which the current `search-kill-ring' originated.")

(defvar pick-backup-eoinput nil
  "Point where minibuffer input ends and file info begins.")

(defvar pick-backup-preview-buffer nil
  "The buffer being used to preview the backup in `pick-backup-file'.")

(defconst pick-backup-preview-buffer-name "*Selected Backup File Preview*")

(defun list-backup-files (filename)
  "Return a list of all available backup files for FILENAME."
  (let* ((filename (file-name-sans-versions
		    (make-backup-file-name (expand-file-name filename))))
	 (file (file-name-nondirectory filename))
	 (dir (file-name-directory filename))
	 (comp (file-name-all-completions file dir)))
    (mapcar '(lambda (x) (concat dir x)) comp)))

(defun pick-backup-file (filename)
  "Prompt to select one of FILENAME's backup files.
The user can browse through the backups with `next-history-element' and
'previous-history-element'."
  (let* ((backup-files (sort (list-backup-files filename)
			     'file-newer-than-file-p))
	 (history (cdr backup-files))
	 (history-length t))
    (setq pick-backup-preview-buffer nil)
    (setq pick-backup-calling-buffer (current-buffer))
    (add-hook 'minibuffer-setup-hook 'pick-backup-minibuffer-setup)
    (unwind-protect
	(completing-read "Pick backup file: " backup-files nil t
			 (car backup-files) 'history)
      (when pick-backup-preview-buffer
	(kill-buffer pick-backup-preview-buffer-name)))))

(defun pick-backup-minibuffer-setup ()
  "Set up the minibuffer for `pick-backup-file'."
  (add-hook 'post-command-hook 'pick-backup-post-command nil t)
  (add-hook 'pre-command-hook 'pick-backup-pre-command nil t)
  (setq pick-backup-eoinput (point-max))
  (with-current-buffer pick-backup-calling-buffer
    (remove-hook 'minibuffer-setup-hook 'pick-backup-minibuffer-setup))
  (setq pick-backup-calling-buffer nil))

(defun pick-backup-format (number string &optional comma)
  (concat (number-to-string number) " " string
	  (when (> number 1) "s")
	  (when comma ", ")))

(defun pick-backup-time-since (time)
  "Format the time since"
  (let* ((diff (time-since time))
	 (seconds (floor (time-to-seconds diff)))
	 (minutes (/ seconds 60))
	 (hours (/ minutes 60))
	 (days (/ hours 24)))
    (concat
     (when (> days 0)
       (pick-backup-format days "day" t))
     (when (> hours 0)
       (pick-backup-format (- hours (* days 24)) "hour" t))
     (when (> minutes 0)
       (pick-backup-format (- minutes (* hours 60)) "minute")))))

(defun pick-backup-post-command ()
  (setq pick-backup-eoinput (point-max))
  (let* ((file-start (eval-when-compile (length "Pick backup file: ")))
	 (current-file (buffer-substring (+ (point-min) file-start)
					 (point-max))))
    (save-excursion
      (if (not (file-exists-p current-file))
	  (insert "\n")
	(let ((change-time (nth 5 (file-attributes current-file))))
	  (insert "\n"
                  (pick-backup-highlight (concat
                                          (format-time-string pick-backup-time-format change-time)
                                          " ("
                                          (concat (pick-backup-time-since change-time) " ago)")
                                          ))
                  ))
	(unless pick-backup-preview-buffer
	  (setq pick-backup-preview-buffer
		(get-buffer-create pick-backup-preview-buffer-name)))
	(with-current-buffer pick-backup-preview-buffer
	  (erase-buffer)
	  (insert-file-contents current-file)
	  (display-buffer (current-buffer)))))))

(defun pick-backup-pre-command ()
  (delete-region pick-backup-eoinput (point-max)))

(defun pick-backup-highlight (hg-info)
  "Highlight the informatin."
  (let* ((hg-info-length (length hg-info)))
    (add-text-properties 0 hg-info-length '(face shadow) hg-info)
    hg-info))

(provide 'pick-backup)
