;;; browse-huge-tar.el --- Browse files in a tarball memory-efficiently.
;;;  $Id: browse-huge-tar.el,v 1.1 2003/11/17 19:44:28 psg Exp $
;;
;; (c) Gareth Owen 1999 (hey I just typed `space' 1999.  Ho ho.)

;; Bug reports, comments, improvements to <lisp@gwowen.freeserve.co.uk> with
;; Subject: "Stop polluting Usenet with your crappy lisp code"
;; Or not, whatever.  Or just recommend your favourite records to me.

;; Latest (yeah, right) version: http://www.geocities.com/drgazowen/lisp/

;; This file is not part of GNU Emacs
;; This is released under the GNU Public License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; if not, write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; "It's that man Edwards! A dramatic start!"
;;              (Cliff Morgan, BBC TV, Barbarians vs.  New Zealand)

;;; Different Commentary:
;;
;; This uses tar (z)tvf to browse a gzipped tar file without opening the
;; whole thing, in a dired-stylee.  Knocked together in a fit of pique
;; after trying to read the xemacs source tarball in xemacs chewed through
;; all my swapspace one afternoon, and as an exercise in thesis avoidance.

;; The trade off is memory usage vs. speed.  This is very slow on large,
;; compressed tarballs, and each operation is slow individually, but
;; relatively low memory machines (like old 486s running one of the i386
;; unices) don't handle these well with jka-compress and tar-mode either.
;; XEmacs-20.4 was a 13MB gzipped tarball and the similarly packaged linux
;; kernel 2.0.36 was 7MB, so the memory savings can be pretty high too.

;; On small files the saving/price is pretty low, and
;; tar-mode/jka-compress have approximately 10^13 more features, so I'd
;; advise you to go that way.

;;; BUGS:
;;  i) Makes some reasonable but sometimes untrue assumptions: e.g.
;;  No spaces in filenames, unless browse-huge-tar-better-heuristics is non-nil
;;  files ending / in tarballs are directories. (The latter may even be true.)
;;  ii) Should perform sanity-checking for directories in
;;  `browse-huge-tar-copy-file-at-point' before the interactive prompt
;;  (using a wrapper and call-interactively?)
;;  iii) Things like default-directory that should probably get set, don't
;;  get set.
;;  iv)  Plenty of others that are probably just hiding.
;;  Bug reports to <lisp@gwowen.freeserve.co.uk>  Did I say that already?

;;  TODO: Option to make the decompressed file stick around to speed up
;;  repeated access at the cost of disk space.  Where would the clean-up
;;  go?  kill-buffer-hook?


;;; History:
;;

;;; Code:
(defconst browse-huge-tar-filename-valid "^ \t"
  "String containing characters that mark the beginning of a filename.

Searched for using `skip-chars-backward'")

(defconst browse-huge-tar-filename-start-column 51
  "Column containing the start of the filename in listing produced by 'tar ztvf'.")


;; These magic bytes come from /usr/share/magic on my GNU/Linux box
;; Corroborated by Kai Grossjohann on comp.emacs
(defconst gzip-magic-bytes '(?\037 . ?\213)
  "Dotted pair of the characters that begin a gzip file.")

(defvar browse-huge-tar-program "tar"
  "Program used for reading the index of tar archives.
Defaults to \"tar\" but may be \"gtar\" on your system.  In all probability,
only those compatible with GNU tar will work")

(defvar browse-huge-tar-file-name nil
  "The filename of the tar file associated with this browse-huge-tar buffer.")
(make-variable-buffer-local 'browse-huge-tar-file-name)

(defvar browse-huge-tar-file-zipped-p nil
  "If non-nil, the tar file is gzipped.")
(make-variable-buffer-local 'browse-huge-tar-file-zipped-p)



;;; Define the interactive functions
;;;###autoload
(defun browse-huge-tar-file (filename)
  "Create a buffer containing a listing of FILENAME as a tar file."
  (interactive "fTar file:")
  ;; Set predictable values for the buffer-local variables
  (setq filename (expand-file-name filename))
  (let ((buf (generate-new-buffer (concat "tar:" filename)))
        (gzipped (browse-huge-tar-gzip-automagic filename)))
    (set-buffer buf)
    (browse-huge-tar-insert-listing filename buf gzipped)
    (switch-to-buffer buf)
    (browse-huge-tar-mode)
    (setq browse-huge-tar-file-name filename
          browse-huge-tar-file-zipped-p gzipped)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

;; One for extracting the file through a pipe into a buffer
;;;###autoload
(defun browse-huge-tar-view-file-at-point ()
  "Extract the file at the point into a buffer for viewing."
  (interactive)
  (let ((filename (browse-huge-tar-get-filename)) buf)
    (setq buf (generate-new-buffer (concat "tar:" filename)))
    ;; Primitive directory detection
    (if (string-match "/$" filename)
        (progn ;; Clean up and abort
          (kill-buffer buf)
          (error (concat filename " appears to be a directory."))))
;;; (call-process PROGRAM &optional INFILE BUFFER DISPLAYP &rest ARGS)
    (call-process browse-huge-tar-program nil
                  buf nil (concat (if browse-huge-tar-file-zipped-p "z") "Oxf")
                  browse-huge-tar-file-name filename)
    (switch-to-buffer buf)
    (let ((buffer-file-name filename))
      (set-auto-mode))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

;;;###autoload
(defun browse-huge-tar-copy-file-at-point (outfile)
  "Extract the file at the point and copy to a local file OUTFILE.
This requires the value of `shell-file-name' to support redirection using \">\"."
  (interactive "FExtract file to: ")
  (setq outfile (expand-file-name outfile))
  ;; FIX Check for directory, provide reasonable suggestion.
  (let ((infile (browse-huge-tar-get-filename)))
    (if (string-match "/$" infile)
        (error (concat infile " appears to be a directory.")))
    (if (file-directory-p outfile)
        (setq outfile (concat outfile "/" infile)))
    (if (or (not (file-exists-p outfile))
            (yes-or-no-p (concat outfile " exists.  Overwrite? ")))
        (progn
          (message "Writing %s..." outfile)
          (shell-command
           (concat "tar" " "
                   (concat (if browse-huge-tar-file-zipped-p "z") "Oxf")
                   " " browse-huge-tar-file-name " " infile " > " outfile))))))


;; Create a keymap
(defvar browse-huge-tar-mode-map nil
  "Local keymap for browse-huge-tar-mode.")
(if browse-huge-tar-mode-map ()
  (setq browse-huge-tar-mode-map (make-keymap))
  (define-key browse-huge-tar-mode-map "\C-m" 'browse-huge-tar-view-file-at-point)
  (define-key browse-huge-tar-mode-map "C" 'browse-huge-tar-copy-file-at-point)
  )

(defvar browse-huge-tar-better-heuristics t
  "This variable controls which filename extracting heuristics to use.

If non-nil, filename fetching is based on browse-huge-tar-filename-start-column
Otherwise, it skips backwards looking for characters in
browse-huge-tar-filename-valid")


;; Define the utility functions
(defun browse-huge-tar-get-filename ()
  "In browse-huge-tar, return name of file mentioned on this line.
Value returned includes all path info associated with the file."
  ;; Compute bol & eol once,
  ;; (bol? Stol^H^H^H^HBorrowed code alert! from dired.el IIRC)
  (let ((eol (save-excursion (skip-chars-forward "^\n\r") (point))))
    (save-excursion
      (if browse-huge-tar-better-heuristics
          (progn (move-to-column browse-huge-tar-filename-start-column)
                 (buffer-substring-no-properties (point) eol))
        (progn (goto-char eol) ;; Else
               (skip-chars-backward browse-huge-tar-filename-valid)
               (buffer-substring-no-properties (point) eol))))))

(defun browse-huge-tar-mode ()
  "Mode for browsing tar files without reading them into memory."
  (kill-all-local-variables)
  (setq major-mode 'browse-huge-tar-mode
        mode-name "Browse-Huge-Tar")
  (use-local-map browse-huge-tar-mode-map))

(defun browse-huge-tar-insert-listing (filename buf &optional gzipped)
  "Insert a listing of the contents of the tar-file FILENAME.

The contents are inserted into buffer BUF.
The optional argument GZIPPED should be non-nil if the tar file is compressed
with GNU gzip."
;;; (call-process PROGRAM &optional INFILE BUFFER DISPLAYP &rest ARGS)
  (let ((errorcode (call-process browse-huge-tar-program nil buf nil
                                 (concat (if gzipped "z") "tvf") filename)))
    (if (or (not (integerp errorcode))
            (not (equal errorcode 0)))
        ;; Then clean up and abort.  Else, keep on keeping on
        (progn (kill-buffer buf)
               (error "Tar process exited abnormally with exit code %s"
                      errorcode)))))

(defun browse-huge-tar-gzip-automagic (filename)
  "Read the first two bytes of file FILENAME and compare with `gzip-magic-bytes'."
  (let ((buf (generate-new-buffer "*browse-huge-tar-tmp*")) retval)
    (save-excursion                     ; Necessary-p?
      (set-buffer buf)
      (insert-file-contents-literally filename nil 0 2)
      (setq retval
            (if (and (char-equal (char-after (point-min))
                                 (car gzip-magic-bytes))
                     (char-equal (char-after (1+ (point-min)))
                                 (cdr gzip-magic-bytes))) t nil))
      (kill-buffer buf)
      (identity retval)))) ;; Is this equiv to 'C' return(retval)?

(provide 'browse-huge-tar)

;;; browse-huge-tar.el ends here
