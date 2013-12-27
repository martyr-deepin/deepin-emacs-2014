;;; shs.el --- facilitate SHell Scripting through Emacs.
;; Time-stamp: <2007-12-06 11:16:28 deego>
;; Copyright (C) 2005 D. Goel
;; Emacs Lisp Archive entry
;; Filename: shs.el
;; Package: shs
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:  0.0
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst shs-home-page
  "http://gnufans.net/~deego/emacspub/lisp-mine/shs/")

;; Copyright (C) 2005 D. Goel


;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.




;; sh.el, posted here a few days ago has been renamed to shs.el since
;; there exists another sh.el -- shs stands for SHell-Script.

;; SHS: Shs aims to facilitate free mixing of elisp with bash: free
;; calls to elisp code from bash shell scripts and calls to other bash
;; scripts from that elisp code, which may again call elisp code and
;; so on, all the while doing the right things as regards bash's error
;; codes, stderr, stdout, etc.

;; Moreover, one shouldn't need to exit emacs just to pipe one emacs
;; script's call to another.

;; Finally, elisp code should also be able to run independently of
;; bash in running emacsen.

;; Provides basic setup for emacs scripting. To the beginning of all
;; emacs shell-scripts, don't forget to add (add-to-list 'load-path
;; directory) and (require 'shs).  Use shs as a convenient way to call
;; shell-commands from the script.  Provides a tutorial on elisp-based
;; shell-scripting.


(eval-when-compile (require 'cl))


;; The most common functions to use are: shs-process (shsp), shs-shell
;; (shsh).

;; Alt, using shell command: shsh.
;; Best way to show messages: shs-message.

;; Your code should automatically run fine, both in shellscripts as
;; well as emacs:


;; The easiest way to pass messages would be to (message) or
;; (princ). However, that makes it somewhat meaningless in running
;; emacs, so prefer using (shs-message) instead.  When using
;; shs-message in running Emacs, all these messages go to *SHS*
;; buffer, which you'll finally want to switch to and see.



;; See also:


;; Quick start:
(defconst shs-quick-start
  "Help..."
  )

(defun shs-quick-start ()
  "Provides electric help from variable `shs-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert shs-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst shs-introduction
  " \(sh.el, posted here a few days ago has been renamed to shs.el
since there exists another sh.el).

shs stands for SHell-Scripting.

I am an utter novice at shell scripting, so suggestions and comments
are most welcome, and please forgive any mistakes in shs. shs aims to
facilitate free mixing of elisp with bash: free calls to elisp code
from bash shell scripts and calls to other bash scripts from that
elisp code, which may again call elisp code and so on, all the while
doing the right things as regards bash's error codes, stderr, stdout,
etc.

Moreover, one shouldn't need to exit emacs just to pipe one emacs
script's call to another.

Finally, ideally, that elisp code should also be able to run
independently of bash in running emacsen.  All that's what shs hopes
to faciliatate.


INSTALLATION: Just add shs.el somewhere in your emacs' load-path.


For a shell scripting  example, (you do need EmacsCVS)


Drop shs.el and the two attached files to a ~/location that is present
both in your emacs' loadpath as well as bash's PATH. Create a
~/.emacs.script with these contents:

 (add-to-list 'load-path \"~/location\")

To be able to use your settings in running emacsen too, also add to
the end of ~/.emacs:  (load \"~/.emacs.script\")

Then, from bash, run shs-example, for a tutorial (I am still learning)
on shell-scripting through Emacs.

Whenever you call shsp instead of shsh, COMMAND is no longer a
string.  It is rather a list whose 1st value is the process, and the
rest of the values are the args.

For script examples to work, you do need emacscvs installed in (or
linked to from) /usr/local/bin/emacscvs.

"
  )

;;;###autoload
(defun shs-introduction ()
  "Provides electric help from variable `shs-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert shs-introduction) nil) "*doc*"))

;;; New features:
(defconst shs-new-features
  "Help..."
  )

(defun shs-new-features ()
  "Provides electric help from variable `shs-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert shs-new-features) nil) "*doc*"))

(defconst shs-version "0.0")

;; Real Code


;; Always make your function
(defmacro shs-exit-code-1 (&rest body)
  "Normally, if the script errors somewhere, Emacs will immediately
exit with an error code of 255, which is the right thing to do.  If
for some reason, you want a different error code, you can wrap this
macro around your lisp code."
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
        (shsm "Error: %S" (error-message-string ,err))
        (kill-emacs 1)))))




(defun shs-sanitize (str)
  "Delete up to one trailing newline from the string.
Typically, shs.el feeds shell commands' output to this function, so
that the result does not have a trailing newline. Is like perl's chop,
  except that this is applied automatically in shs"

  (replace-regexp-in-string
   "\n\\'" "" (format "%s" str)))

(defalias 'shs-chop 'shs-sanitize)



(defalias 'shs-shell-exit 'shs-shell-command-with-exit)

(defvar shs-shell-buffer "*SHS-SHELL*")
(defvar shs-process-buffer "*SHS-PROC*")
(get-buffer-create shs-shell-buffer)
(get-buffer-create shs-process-buffer)

;;;###autoload
(defun shs-shell-command (command &rest args)
  "Shell commands from a running script, exit on errors.

NOT suitable for asynchronous shell commands.  If everything ok,
then return the result of the shell-command as a string, else
exit emacs with the same exit code.

COMMAND shou]d be a string.  You can also give us the command in
the shsp format: a list.  In that case, we shall try to guess the
command by converting it to a string by concatting the
shell-quote-argument for each argument.  But note that shsp might
be safer.
"
  (shsh--reset-buffer)
  (setq command (shs-convert-command-list-to-string-maybe command))
  (let ((code (apply 'shell-command command shs-shell-buffer nil))
        (output
         (with-current-buffer shs-shell-buffer
           (shs-sanitize
            (buffer-substring (point-min) (point-max))))))

    (cond
     ((equal code 0)
      output)
     ;; as you see, the string-to-number of this error code will
     ;; always be the correct error code.
     (t (error "%S -- error code when trying %S\n Output was: %S" code
               command output)))))

(defun shs-convert-command-list-to-string-maybe (c)
  (if (stringp c)
      c
    (shs-convert-command-list-to-string c)))

(defun shs-convert-command-list-to-string  (c)
  (mapconcat 'shell-quote-argument
             c  " "))


(defun shs-convert-command-string-to-list-maybe (c)
  (if (listp c)
      c
    (shs-convert-command-string-to-list c)))

(defun shs-convert-command-string-to-list (c)
  (split-string c))


;;;###autoload
(defun shs-shell-command-and-code (command &rest args)
  (shsh--reset-buffer)
  (let ((code (apply 'shell-command command shs-shell-buffer nil)))
    (list code
          (with-current-buffer shs-shell-buffer
            (shs-sanitize
             (buffer-substring (point-min) (point-max)))))))




(defun shsp--reset-buffer ()
  (with-current-buffer shs-process-buffer
    (delete-region (point-min) (point-max))))

(defun shsh--reset-buffer ()
  (with-current-buffer shs-shell-buffer
    (delete-region (point-min) (point-max))))

;;;###autoload
(defun shs-process-and-code (command &optional infile)
  (shsp--reset-buffer)
  (let ((code (apply 'call-process (car command)  infile shs-process-buffer
                     nil (cdr command))))
    (list code
          (with-current-buffer shs-process-buffer
            (shs-sanitize
             (buffer-substring (point-min) (point-max)))))))

;;;###autoload
(defun shs-process (command &optional infile instring outfile appendp)
  "process from a running script, exit on errors.

NOT suitable for asynchronous processes.  If everything ok,
then return the result of the shell-command as a string, else
error with the same exit code.

COMMAND shou]d be a list.  You can also give us the command in
the shsh format: a string.  In that case, we shall convert it to
a list by taking every word in that string.  But note that list
might be safer.

Both infile and instring can be nil, in which case, no stdin is passed
to the process.

If INFILE is non-nil it is used.  If INFILE is nil and INSTRING is
not, we put instring in a temporary file, and use that as the
stdin. This is kinda like bash's <.

If outfile is non-nil, the output is also written to outfile.  If
appendp is non-nil, the output is appended to any preceding output.
These were kinda like bash's > and >>.

pseudo-Pipes can be accomplished via use of instring.  See, for
example, `shsu-pipe'.
"
  (let ((rmp (and (not infile) instring)))
    (setq command (shs-convert-command-string-to-list-maybe command))
    (when rmp
      ;; see also, for example, shsu-mktemp-d
      (setq infile (shsp "mktemp"))
      (with-temp-buffer
        (insert instring)
        (let ((require-final-newline nil))
          (write-file infile nil))))
    (let* ((codeoutput (shs-process-and-code command infile))
           (code (car codeoutput))
           (output (cadr codeoutput)))
      (when rmp (delete-file infile))
      (cond
       ((equal code 0)
        (when outfile
          (with-temp-buffer
            (when (and appendp (file-exists-p outfile))
              (insert-file-contents outfile))
            (goto-char (point-max))
            (insert output)
            (let ((require-final-newline nil))
              (write-file outfile nil))))
        output)
       ;; as you see, the string-to-number of this error code will
       ;; always be the correct error code.
       (t (error "%S -- error code when call-process: %S\n Output was: %S" code command output))))))





(defmacro shs-ignore-errors-flag (&rest body)
  "Copied from ignore-errrors-my.

which: Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
        (ding t)
        (ding t)
        (ding t)
        (shsm "IGNORED ERROR: %s" (error-message-string ,err))
        (sit-for 1)
        nil))))








;;;###autoload
(defalias 'shs-shell 'shs-shell-command)


;;;###autoload
(defalias 'shsh 'shs-shell-command)


;;;###autoload
(defalias 'shsp 'shs-process)

;;;###autoload
(defalias 'shs-call-procell 'shs-process)

(defun shs-shell-command-with-error (&rest args)
  "NOT USED ANY MORE.
Shell commands from a running script, exit on errors.

NOT suitable for asynchronous shell commands.  If everything ok,
then return the result of the shell-command as a string, else
exit emacs with the same exit code.
"
  (let ((code (apply 'shell-command args)))
    (cond
     ((equal code 0)
      (with-current-buffer shs-shell-buffer
        ;;(buffer-substring-no-properties (point-min) (point-max))))
        (shs-sanitize
         (buffer-substring (point-min) (point-max)))))
     (t (error "Bash Error code: %S" code)))))



(defvar shs-buffer "*SHS*")
(get-buffer-create shs-buffer)

(defvar shs-message-sit-for 0.1
  "We wait for this duration at critical points when using shs.
Matters only when called within emacs. ")

(defun shs-message (&rest args)
  (cond
   (noninteractive
    (apply 'message  args))
   (t
    (save-excursion
      (set-buffer (get-buffer-create shs-buffer))
      (goto-char (point-max))
      (insert "\n")
      (insert (apply 'format args))
      (message
       "%s"
       (apply 'format args)

       ;;"Note: This message is saved in the *SHS* and *messages*
       ;;buffer."
       )
      (sit-for shs-message-sit-for)))))



(defalias 'shs-msg 'shs-message)
(defalias 'shsm 'shs-message)


;; OBSOLETE
(defalias 'shs-shell-error 'shs-shell-command-with-error)


(defun shs-display-buffer ()
  (display-buffer shs-buffer)
  (let ((cur (current-buffer)))
    (set-buffer shs-buffer)
    (goto-char (point-max))
    (set-buffer cur)))


(defvar shs-bye-hook (list 'shs-display-buffer))

(defun shs-bye ()
  (interactive)
  (run-hooks 'shs-bye-hook)
  )

(defun shs-clear-buffer ()
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create shs-buffer))
    (erase-buffer)))

(defvar shs-start-hook (list 'shs-clear-buffer))
(defun shs-start ()
  "For use when using shs from eshell. "
  (interactive)
  (run-hooks 'shs-start-hook))

(defvar shs-within-p nil
  "When non-nil, start and end-hooks are NOT executed..  May be useful
to set via the `shs-within' macro one \"top-level\" shs function is
calling another..

In the default setting, this matters only when shs functions are used
from within emacs")

(defmacro shs-within (&rest args)
  `(let ((shs-within-p t))
     (progn ,@args)))


(defun shs-help (g)
  "Call this function with your function name."
  (shsm "")
  (shsm
   "Running this script calls the Emacs function described below.")
  (shsm
   (let* ((def (symbol-function g)))
     (ignore-errors
       (if (equal 'autoload (car-safe def))
           (load (second def))))
     ;; this check does nothing now.. need ro

     (describe-function g))))



(defun shs-help-check (args)
  (let
      ((argstr
        (mapconcat
         (lambda (a) (format "%s" a))
         args
         " ")))
    (or
     (string-match "\\b-h\\b" argstr)
     (string-match "help" argstr))))


(defun shs-shell-flag (command &rest args)
  "
Added back, since used by some of my scripts. "
  (let ((coderes (apply 'shs-shell-command-and-code  command args)))
    (when (not (equal (first coderes) 0))
      (shsm  "IGNORED: ERROR CODE: %S WHEN TRYING %S " (first coderes)
             command))
    (second coderes)))


;;;###autoload
(defun shs-expand-file-name (file dired)
  "Copied from utils-expand-file-name.

Suggested by Paul Jarc on g.e.d.  in 2005-07 when I raised this
issue:

Emacs' default expand-file-name is slightly borked, the bork can be
seen if there is a file or a directory literally named ~.  The bork
comes from the emacs-tilde-feature: anywhere emacs sees a ~ in a
path, it drops the entire preceding path and starts from /home/$USER
afresh.

viz. Create a file ~/tmp/~. Then
 \(expand-file-name (file-name-nondirectory  \"~/tmp/~\")
                 \(file-name-directory \"~/tmp/~\"))
is incorrect

The version below avoids that problem, but of course, it lacks
the emacs-tilde-feature.  It is also portable across platforms,
including VMS.

However, note that this function is not necc. conformant with expand-file-name
as far as argument structure and all function features are concerned.
"
  (concat (file-name-as-directory dired)
          (file-name-nondirectory  file)))






(provide 'shs)


