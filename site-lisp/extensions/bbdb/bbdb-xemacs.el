;;; -*- Mode:Emacs-Lisp -*-
;;; This file contains some XEmacs-specific stuff for BBDB.

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This code is kind of kludgey, mostly because it needs to parse the contents
;;; of the *BBDB* buffer, since BBDB doesn't save the buffer-positions of the
;;; various fields when it fills in that buffer (doing that would be slow and
;;; cons a lot, so it doesn't seem to be worth it.)

(or (string-match "XEmacs\\|Lucid" emacs-version)
    (error "This file only works in XEmacs."))

;; this makes no sense, long-term, but.
(eval-when-compile
  (or (featurep 'xemacs)
      (fset 'load-sound-file 'ignore)))

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-gui) ;; load in the menu/font stuff

;; Utility functions that mask others to provide XEmacs-specific functionality
;;;###autoload
(defun bbdb-xemacs-display-completion-list (list &optional callback data)
  "Wrapper for `display-completion-list'.
Allows callbacks on XEmacs `display-completion-list' is called with
`:activate-callback CALLBACK' if CALLBACK is non-nil.
`:user-data DATA' is also used if DATA is non-nil.
Neither are used if CALLBACK is nil."
  (cond ((and callback data)
         (display-completion-list list
                                  :activate-callback callback
                                  :user-data data))
        (callback
         (display-completion-list list
                                  :activate-callback callback))
        (t
         (display-completion-list list))))


;; For native Xemacs sound support we can use these ...
;;;###autoload
(defcustom bbdb-sounds-directory (expand-file-name "~/.xemacs/etc/sounds")
  "The directory to load the touchtone sound files from, or nil if none."
  :group 'bbdb-phone-dialing
  :type 'directory)

;;;###autoload
(defcustom bbdb-sound-volume 50
  "Volume for playing sounds."
  :group 'bbdb-phone-dialing
  :type 'integer)

;;;###autoload
(defun bbdb-load-touchtones ()
  "Load the touchtone sounds into `sound-alist'.
The directory specified in `bbdb-sounds-directory' is searched for the files
touchtone.*\\.\\(wav\\|au\\) as named in `bbdb-sound-files'.
They are stored in `sound-alist' as touchtone0 to touchtone11."
  (interactive)
  (let (files
        (nr 0))
    (condition-case nil
        (setq files
              (directory-files bbdb-sounds-directory t
                               (if (and system-type
                                        (string-match
                                         "windows"
                                         (format "%s" system-type)))
                                   "touchtone.*\\.wav"
                                 "touchtone.*\\.au")))
      (error
       ;; It is not a fatal error if we can't find the touchtones; it
       ;; just prevents a particular, possibly little-used feature
       ;; from working.
       (bbdb-warn "Cannot find any touchtone sounds")
       (setq files nil)))

    (if (not files)
        (progn
          (message "No touchtone files found in `bbdb-sound-directory'!")
          (sit-for 2))
      ;; otherwise, load 'em up.
      (while files
        (load-sound-file (car files)
                         (intern (concat "touchtone" (format "%d" nr)))
                         bbdb-sound-volume)
        (setq files (cdr files)
              nr (1+ nr))))))

(if (and bbdb-sounds-directory
         (file-directory-p bbdb-sounds-directory)
         (boundp 'xemacsp)
         (featurep 'native-sound))
    (bbdb-load-touchtones))

(provide 'bbdb-xemacs)
