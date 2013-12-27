;;; cheezburger.el --- ICanHasCheezburger.com viewer for emacs!

;; Copyright (C) 2008 Zachary McGrew 

;; Author: Zachary McGrew; cheezburger.el <zmcgrew @-A-T-@ lunar-linux.org>
;; Keywords: cheezburger, lolcat
;; Created: 19 Nov 2008
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x cheezburger
;; Now with 100% more cheezy goodness!

;; This is my first experiment with elisp. Comments and criticisms welcome!

;;; Future Ideas:

;; Display title of image (requires parsing HTML &codes;)
;; Add support for the rest of the I Can LOL network

;;; Compatibility:

;; Only tested on GNU Emacs 23.0.60.1
;; Should work on 21+ if you have json and url

;;; Code:

;I can has cheezburger?
(provide 'cheezburger)
;Nom nom nom! Is delishus!

(require 'json)
(require 'url)

(defun cheez-setup-keymap ()
  (setq cheez-map (make-sparse-keymap))
  (suppress-keymap cheez-map)
  (define-key cheez-map "n" 'cheezburger-next-image)
  (define-key cheez-map "p" 'cheezburger-prev-image)
  (define-key cheez-map "i" 'cheezburger-kill-image-url)
  (define-key cheez-map "u" 'cheezburger-kill-url)
  (define-key cheez-map "q" 'cheezburger-quit)
  (use-local-map cheez-map))

(define-derived-mode cheezburger-mode nil "cheez"
  "Major mode for I Can Has Cheezburger.
          \\{cheez-map}"
  (make-local-variable 'cheez-page)
  (make-local-variable 'cheez-current-image-num)
  (make-local-variable 'cheez-num-images)
  (make-local-variable 'cheez-data)
  (make-local-variable 'cheez-image-data)
  (setq cheez-page 1)
  (setq cheez-current-image-num 0)
  (setq cheez-num-images 0)
  (setq cheez-data nil)
  (setq cheez-image-data nil)
  (cheez-setup-keymap))

(defun cheezburger ()
  (interactive)

  ;Create the buffer to display the image
  (get-buffer-create "*cheezburger*")
  (switch-to-buffer "*cheezburger*")
  (cheezburger-mode)
  (cheezburger-get-data)

  ;Cheat and let it advance to the first image =)
  (setq cheez-current-image-num -1)
  (cheezburger-next-image)
)

(defun cheezburger-get-data () 
  "Get data from the icanlol.com server, parse it, and prepare to show images"
  (let ((cheez-json
	 (save-excursion
	   (set-buffer (url-retrieve-synchronously (format "http://www.icanlol.com/cheezburger.php?page=%d" cheez-page)))
	   (progn (goto-char (point-min))
		  (delete-region (point-min) (search-forward "\n\n"))
		  (buffer-substring (point-min) (point-max))))))
    (setq cheez-current-image-num 0)
    (setq cheez-data (json-read-from-string cheez-json))
    (setq cheez-num-images (length cheez-data))))

(defun cheezburger-next-image ()
  "Fetch and display the next image"
  (interactive)

  (setq cheez-current-image-num (1+ cheez-current-image-num))

  (if (>= cheez-current-image-num cheez-num-images)
      (progn
	;Incement cheez-page and fetch next batch of data
	(setq cheez-page (1+ cheez-page))
	(cheezburger-get-data)))

  (setq cheez-image-url (cdr (assoc 'url (aref cheez-data cheez-current-image-num))))
  ;Fetch image and display it here
  (cheezburger-disp-image))

(defun cheezburger-prev-image ()
  "Fetch and display the previous image"
  (interactive)

  (setq cheez-current-image-num (1- cheez-current-image-num))
  
  (if (< cheez-current-image-num 0)
      (progn
	;Decrement cheez-page and fetch previous batch of data
	(setq cheez-page (1- cheez-page))
	(if (< cheez-page 1)
	    (progn
	      (setq cheez-page 1)
	      (message "Already at the first image!"))) ;Should return from this point, but I don't know how
	(cheezburger-get-data)
	(setq cheez-current-image-num (1- (length cheez-data)))))

  (setq cheez-image-url (cdr (assoc 'url (aref cheez-data cheez-current-image-num))))
  ;Fetch image and display it here
  (cheezburger-disp-image))

(defun cheezburger-disp-image ()
  "Fetch and display the current image in the buffer"
  (setq cheez-image-data
	 (save-excursion
	   (set-buffer (url-retrieve-synchronously cheez-image-url))
	   (progn (goto-char (point-min))
		  (delete-region (point-min) (search-forward "\n\n"))
		  (buffer-substring (point-min) (point-max)))))

  (delete-region (point-min) (point-max))

  (let ((cheez-image (create-image cheez-image-data nil t)))
    (insert-image cheez-image)))

(defun cheezburger-kill-image-url ()
  "Copy the URL of the image"
  (interactive)
  (kill-new (cdr (assoc 'url (aref cheez-data cheez-current-image-num))))
  (message "URL to image inserted into kill ring"))

(defun cheezburger-kill-url ()
  "Copy the URL of the actual blog post"
  (interactive)
  (kill-new (cdr (assoc 'permalink (aref cheez-data cheez-current-image-num))))
  (message "URL to post inserted into kill ring"))

(defun cheezburger-quit ()
  "Close the cheezburger buffer, and continue with real work."
  (interactive)
  (kill-buffer "*cheezburger*"))

;;; cheezburger.el ends here
