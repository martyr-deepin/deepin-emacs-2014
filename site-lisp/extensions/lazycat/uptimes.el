;;; uptimes.el --- Track and display emacs session uptimes.
;; Copyright 1999,2000,2001,2002 by Dave Pearson <davep@davep.org>
;; $Revision: 2.3 $

;; uptimes is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; uptimes provides a simple system for tracking and displaying the uptimes
;; of your emacs sessions. Simply loading uptimes.el from your ~/.emacs file
;; will start the tracking of any session.
;;
;; The latest version of uptimes.el can be found at:
;;
;;   <URL:http://www.davep.org/emacs/uptimes.el>

;;; TODO:
;;
;; o The auto-update stuff won't work with xemacs because I'm making use of
;;   `run-at-time'. If anyone knows of a high-level function that does the
;;   same thing and is available in GNU emacs and xemacs I'd love to hear
;;   about it.

;;; THANKS:
;;
;; Istvan Marko <imarko@pacificnet.net> for pointing out that a default of
;; one second for `uptimes-auto-save-interval' was probably a little OTT.
;;
;; Doug Elias <dme7@cornell.edu> for pointing out that midnight.el is a
;; recent addition to emacs.
;;
;; Nix <nix@esperi.demon.co.uk> for pointing out that some XEmacs users
;; might need `inhibit-clash-detection' set to t at points in this code.

;;; Code:

;; Bits that we need.

(eval-when-compile
  (require 'cl))
(require 'pp)
(require 'timer)

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Because cl is only required at compile time we'll do the alias for
;; `values' ourself during run-time.
(eval-when (load eval)
  (unless (fboundp 'values)
    (defalias 'values 'list)))

;; Create a version constant.

(defconst uptimes-version
  (let ((revision "$Revision: 2.3 $"))
    (when (string-match ": \\([0-9.]+\\)" revision)
      (match-string 1 revision)))
  "uptimes version.")

;; Customize options.

(defgroup uptimes nil
  "Track emacs session uptimes."
  :group 'games
  :prefix "uptimes-")

(defcustom uptimes-database "~/.emacs-uptimes"
  "*Database of uptimes."
  :type  'file
  :group 'uptimes)

(defcustom uptimes-keep-count 10
  "*Number of uptimes to keep."
  :type  'integer
  :group 'uptimes)

(defcustom uptimes-auto-save t
  "*Should we auto-save our uptime data?"
  :type  '(choice (const :tag "Yes, auto-save uptime details" t)
                  (const :tag "No, don't auto-save details" nil))
  :group 'uptimes)

(defcustom uptimes-auto-save-interval 300
  "*How often, in seconds, should we auto-save the data?"
  :type  'integer
  :group 'uptimes)

;; The following functions are borrowed from midnight.el. I've made copies
;; here for two reasons. First, older versions of emacs don't have
;; midnight.el, second, (require 'midnight) has side-effects that some
;; people might not want. Somehow, this "cut-n-paste" method of coding
;; doesn't quite seem right, perhaps I'm missing something?

(defun uptimes-float-time (&optional tm)
  "Convert `current-time' to a float number of seconds."
  (multiple-value-bind (s0 s1 s2) (or tm (current-time))
    (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2))))

(defun uptimes-time-float (num)
  "Convert the float number of seconds since epoch to the list of 3 integers."
  (let* ((div (ash 1 16))
         (1st (floor num div)))
    (list 1st (floor (- num (* (float div) 1st)))
          (round (* 10000000 (mod num 1))))))

;; Non-customize variables.

(defvar uptimes-boottime (uptimes-float-time)
  "The time that uptimes.el came into existance.")

(defvar uptimes-last-n nil
  "Last `uptimes-keep-count' uptimes.")

(defvar uptimes-top-n nil
  "Top `uptimes-keep-count' uptimes.")

(defvar uptimes-auto-save-timer nil
  "Timer object for the auto-saver.

Note that the timer object isn't used in the uptime code but this variable
is probided so that you can kill/restart the timer in your own code.")

;; Main code.

(defun* uptimes-key (&optional (boottime uptimes-boottime))
  "Return an `assoc' key for the given BOOTTIME.

If not supplied BOOTTIME defaults to `uptimes-boottime'."
  (format "%.7f" boottime))

(defun* uptimes-uptime (&optional (boottime uptimes-boottime)
                                  (endtime (uptimes-float-time)))
  "Return the uptime of BOOTTIME at ENDTIME."
  (- endtime boottime))

(defun* uptimes-uptime-values (&optional (boottime uptimes-boottime)
                                         (endtime (uptimes-float-time)))
  "Get the different parts of an uptime.

BOOTTIME is an optional boot-time for an emacs process, if not supplied the
default is the boot-time of the current process. ENDTIME is the optional
time at which the emacs process closed down, if not supplied the default is
the current time.

The result is returned as the following `values':

  (DAYS HOURS MINS SECS)"
  (let* ((now   (uptimes-uptime boottime endtime))
         (days  (floor (/ now 86400)))
         (hours (progn (decf now (* days  86400)) (floor (/ now 3600))))
         (mins  (progn (decf now (* hours 3600))  (floor (/ now 60))))
         (secs  (progn (decf now (* mins  60))    (floor now))))
    (values days hours mins secs)))

(defun* uptimes-uptime-string (&optional (boottime uptimes-boottime)
                                         (endtime (uptimes-float-time)))
  "Return `uptimes-uptime-values' as a human readable string."
  (multiple-value-bind (days hours mins secs)
      (uptimes-uptime-values boottime endtime)
    (format "%d.%02d:%02d:%02d" days hours mins secs)))

(defun* uptimes-wordy-uptime (&optional (boottime uptimes-boottime)
                                        (endtime (uptimes-float-time)))
  "Return `uptimes-uptime-values' as a \"wordy\" string."
  (multiple-value-bind (days hours mins secs)
      (uptimes-uptime-values boottime endtime)
    (cl-flet ((mul (n word) (concat word (unless (= n 1) "s")))
           (say (n word) (format "%d %s" n (mul n word))))
      (concat (say days "day")
              ", "
              (say hours "hour")
              ", "
              (say mins "minute")
              " and "
              (say secs "second")))))

(defun uptimes-read-uptimes ()
  "Read the uptimes database into `uptimes-last-n' and `uptimes-top-n'."
  (when (file-exists-p uptimes-database)
    (with-temp-buffer
      (let ((inhibit-clash-detection t)) ; For the benefit of XEmacs.
        (insert-file-contents uptimes-database t))
      (setq uptimes-last-n (read (current-buffer)))
      (setq uptimes-top-n  (read (current-buffer))))))

(defun uptimes-update ()
  "Update `uptimes-last-n' and `uptimes-top-n'."
  (uptimes-read-uptimes)
  (cl-flet ((trunc (list &optional (where uptimes-keep-count))
                (let ((trunc-point (nthcdr (1- where) list)))
                  (when (consp trunc-point)
                    (setf (cdr trunc-point) nil)))
                list)
         (update (list now sort-pred)
                 (let* ((key  (uptimes-key))
                        (this (cdr (assoc key list))))
                   (unless this
                     (setq this (cons uptimes-boottime nil))
                     (push (cons key this) list))
                   (setf (cdr this) now)
                   (trunc (sort list sort-pred)))))
    (let ((now (uptimes-float-time)))
      (setq uptimes-last-n
            (update uptimes-last-n now
                    (lambda (x y) (> (cddr x) (cddr y)))))
      (setq uptimes-top-n
            (update uptimes-top-n now
                    (lambda (x y)
                      (> (uptimes-uptime (cadr x) (cddr x))
                         (uptimes-uptime (cadr y) (cddr y)))))))))

(defun uptimes-save-uptimes ()
  "Write the uptimes to `uptimes-database'."
  (uptimes-update)
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (pp uptimes-last-n)
      (pp uptimes-top-n)
      ;; TODO: What is the correct method of ignoring a lock error (IOW,
      ;; don't bother trying to write if there is a locking problem)?
      (let ((inhibit-clash-detection t)) ; For the benefit of XEmacs.
        (write-region (point-min) (point-max) uptimes-database nil 0)))))

(defun uptimes-print-uptimes (list)
  "Print uptimes list LIST to `standard-output'."
  (princ "Boot                Endtime             Uptime       This emacs\n")
  (princ "=================== =================== ============ ==========\n")
  (cl-flet ((format-time (time)
                      (format-time-string "%Y-%m-%d %T" (uptimes-time-float time))))
    (loop for uptime in list
          for bootsig  = (car  uptime)
          for booted   = (cadr uptime)
          for snapshot = (cddr uptime)
          do (princ (format "%19s %19s %12s %s\n"
                            (format-time booted)
                            (format-time snapshot)
                            (uptimes-uptime-string booted snapshot)
                            (if (string= bootsig (uptimes-key)) "<--" ""))))))

;;;###autoload
(defun uptimes ()
  "Display the last and top `uptimes-keep-count' uptimes."
  (interactive)
  (uptimes-save-uptimes)
  (with-output-to-temp-buffer "*uptimes*"
    (princ (format "Last %d uptimes\n\n" uptimes-keep-count))
    (uptimes-print-uptimes uptimes-last-n)
    (princ (format "\nTop %d uptimes\n\n" uptimes-keep-count))
    (uptimes-print-uptimes uptimes-top-n)))

;;;###autoload
(defun uptimes-this ()
  "Display the uptime for the current emacs session."
  (interactive)
  (uptimes-save-uptimes)
  (message "emacs has been up and running for %s" (uptimes-wordy-uptime)))

;; Register our presence and, if `uptimes-auto-save' is true, kick off the
;; auto-save process.

(eval-when (load eval)
  (uptimes-save-uptimes)
  (when uptimes-auto-save
    (setq uptimes-auto-save-timer
          (run-at-time nil uptimes-auto-save-interval #'uptimes-save-uptimes)))
  (add-hook 'kill-emacs-hook #'uptimes-save-uptimes))

(provide 'uptimes)

;;; uptimes.el ends here
