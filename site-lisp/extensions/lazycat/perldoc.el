;;; perldoc.el --- Show help for Perl functions, builtins, and modules.

;;
;; Copyright (C) 2000-2002 Steve Kemp <skx@tardis.ed.ac.uk>
;; Copyright (C) 2003, 2005 Peter S Galbraith <psg@debian.org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;  This package allows the user to view the Perl help for the word(s) at
;;  the point.
;;
;;  When this is loaded it adds a hook both `cperl-mode', and `perl-mode',
;; allowing the perldoc help to be shown for the thing under the point, by
;; pressing F1.
;;
;;  The code handles functions, builtins, and third party modules.

;;; Version History
;;
;;  1.0 - Initial Release.
;;  1.1 - Show error message when no help is found.
;;      - Fix name.
;;      - Include GPL + URL.
;;  1.2 Alan Shutko <ats@acm.org>
;;        perldoc runs a pager, so run a benign one.  See Debian bug
;;        http://bugs.debian.org/144963
;;  1.3 Peter S Galbraith <psg@debian.org>
;;      - Checkdoc clean.
;;      - Generate list of functions on the fly instead of using a
;;        hardwired list.
;;      - ToDo? Allow completion on module names, harvested from all .pod
;;              file under directories in @INC.
;;  1.4 Peter S Galbraith <psg@debian.org>
;;      - Handle case where Debian perldoc package is not installed.
;;        Thanks to Kevin Ryde <user42@zip.com.au> for the full bug report.
;;
;;  1.5 Peter S Galbraith <psg@debian.org>
;;      - Apply patch from Kevin Ryde (Closes: #314869)


;;  Comments / suggests / feedback welcomed to skx@tardis.ed.ac.uk

;;; Code:

(require 'thingatpt)

(autoload 'Man-fontify-manpage "man")

(defgroup perldoc nil
  "Show help for Perl functions, builtins, and modules."
  :group  'help)

(defcustom perldoc-define-F1 nil
  "If non-nil, bind [F1] to `perl-doc-at-point' in perl modes.
It installs `perldoc-perl-hook' in Perl mode hooks."
  :type 'boolean
  :group 'perldoc
  :require 'perldoc
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond
          (value
           (add-hook 'cperl-mode-hook 'perldoc-perl-hook)
           (add-hook 'perl-mode-hook 'perldoc-perl-hook))
          (t
           (remove-hook 'cperl-mode-hook 'perldoc-perl-hook)
           (remove-hook 'perl-mode-hook 'perldoc-perl-hook)))))

(defvar perldoc-functions-alist nil
  "Alist holding the list of perl functions.")

(defun perldoc-functions-alist ()
  "Return the alist of perl functions constructed from perlfunc.pod."
  (if perldoc-functions-alist
      perldoc-functions-alist
    (setq perldoc-functions-alist nil)
    (let ((tmp-buffer (get-buffer-create " *perldoc*"))
          (case-fold-search nil))
      (set-buffer tmp-buffer)
      (erase-buffer)
      (shell-command "perldoc -u perlfunc" t)
      (goto-char (point-min))
      (cond
       ((search-forward "Alphabetical Listing of Perl Functions" nil t)
        (while (re-search-forward
                "^=item \\(\\([a-z][^ //\n]*\\)\\|\\(I<\\(.*\\)> \\)\\)" nil t)
          (let ((entry (list (or (match-string 2)(match-string 4)))))
            (when (not (member entry perldoc-functions-alist))
              (push entry perldoc-functions-alist))))
        ;; no output means the perldoc program doesn't exist or is only the
        ;; debian perl package dummy script
        (unless perldoc-functions-alist
          (error "`perldoc' program not available"))
        perldoc-functions-alist)
       ((re-search-forward "You need to install.*" nil t)
        (error (format "%s" (match-string 0))))
       (t
        (error "`perldoc' program not available"))))))

;;;###autoload
(defun perldoc (string)
  "Run perldoc on the given STRING.
If the string is a recognised function then we can call `perldoc-function',
otherwise we call `perldoc-module'."
  (interactive (list (completing-read "Perl function or module: "
                                      (perldoc-functions-alist) nil nil)))
  (perldoc-functions-alist)
  (cond
   ((assoc string perldoc-functions-alist)
    (perldoc-function string))
   ((stringp string)
    (perldoc-module string))
   (t
    (message "Nothing to find."))))
    
(defun perldoc-start-process (&rest args)
  "Call perldoc with ARGS.
Sets up process sentinals and needed environment to call perldoc."
  (let* ((pager (if (member system-type '(ms-dos windows-nt))
		    "type"
		  "cat"))
	 (perldoc-process)
	 (process-environment
	  (cons (concat "PERLDOC_PAGER=" pager)
	  process-environment)))
    ;; Can't convince perldoc not to run a pager, so we run a
    ;; benign one
    (set-buffer (get-buffer-create "*Perldoc*"))
    (kill-all-local-variables)
    (erase-buffer)
    (text-mode)
    (message "Loading documentation ..")
    (setq perldoc-process (apply 'start-process args))
    (set-process-filter perldoc-process 'perldoc-process-filter)
    (set-process-sentinel perldoc-process 'perldoc-sentinel)
    (process-kill-without-query perldoc-process)))
    

(defun perldoc-function (function)
 "Show the help text for the given Perl FUNCTION / builtin."
 (interactive (list (completing-read "Perl function: "
                                     (perldoc-functions-alist) nil t)))
 (perldoc-start-process "perldol" nil "perldoc" "-f" function))

(defun perldoc-module (module)
 "Show the help text for the given Perl MODULE."
 (interactive "sPerl module : ")
   (perldoc-start-process "perldol" nil "perldoc" module))

(defun perldoc-process-filter (proc string)
  "Process the results from the catdoc process PROC, inserting STRING."
  (set-buffer (get-buffer-create "*Perldoc*"))
  (insert string))

(defun perldoc-sentinel (proc msg)
  "Perldoc sentinel for process PROC and MSG describing the change.
When the catdoc process has finished, switch to its output buffer."
  (cond ((eq (process-status proc) 'exit)
	 (set-buffer "*Perldoc*")
         (goto-char (point-min))
         (cond
          ((and (< (count-lines (point-min) (point-max)) 2)
                (re-search-forward "No documentation found for .*" nil t))
           (message (match-string 0))
           (kill-buffer (get-buffer "*Perldoc*")))
          (t
	   (pop-to-buffer "*Perldoc*")
	   (goto-char (point-min))
	   (let ((Man-args "perldoc"))
	     (Man-fontify-manpage)))))))

;;;###autoload
(defun perldoc-at-point ()
  "Call `perldoc' for string at point."
  (interactive)
  (perldoc (or (thing-at-point 'word)
               (thing-at-point 'filename))))

;;;###autoload
(defun perldoc-perl-hook ()
  "A hook which binds F1 to `perldoc-at-point'."
  (local-set-key [f1] 'perldoc-at-point))

(provide 'perldoc)
;;; perldoc.el ends here
