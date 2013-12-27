;; Routines to do devscripts-compatible emacs routines.
;; copyright 2002 Junichi Uekawa.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; readme-debian.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

(require 'mcharset)
(defgroup pbuilder-log-view nil "Pbuilder log view mode"
  :group 'tools
  :prefix "pbuilder-log-view-")

(defcustom pbuilder-log-view-web-basepath "/~pbuilder/" "*Elserv path to pbuilder logs."
  :type 'string
  :group 'pbuilder-log-view)

(defvar pbuilder-log-view-build-result-alist nil
  "Associated list of results of the pbuilder/debuild runs.
They are in (package result-buffer-name process-name(if process exists))

When this variable is being accessed, set `pbuilder-log-view-build-result-alist-mutex' to t.")

(defvar pbuilder-log-view-build-result-alist-mutex nil
  "The access-control for `pbuilder-log-view-build-result-alist'.
If someone is accessing that var, it is t")

;; potential new interface?
(defvar pbuilder-log-view-results-plist nil
  "Property list of results of the pbuilder/debuild runs.
:package
:result-buffer-name
:process-name")

;; mutex lock implementation thanks to TSUCHIYA Masatoshi
(defmacro pbuilder-log-view-lock-mutex (mutex &rest body)
  "Try to mutex-lock a variable MUTEX, and run BODY.

The MUTEX needs to be nil."
  `(progn
     (while ,mutex
       (accept-process-output nil 0 200))
     (setq ,mutex t)
     ,@body
     (setq ,mutex nil)))

;; The following code does publishing for elserv.
;; elserv-start, then run pbuilder-log-view-elserv

(defun pbuilder-log-view-add (package-name buffer-name running-process)
  "Add the entry to the log view list.

\(PACKAGE-NAME, BUFFER-NAME, RUNNING-PROCESS\) will be added to
`pbuilder-log-view-build-result-alist'.

Argument PACKAGE-NAME is the name of the package."
  (pbuilder-log-view-lock-mutex
   pbuilder-log-view-build-result-alist-mutex
   (add-to-list 'pbuilder-log-view-build-result-alist (list package-name buffer-name running-process))))

(defun pbuilder-log-view-internal-garbage-collect-log ()
  "Remove unneeded entries from the log listing."
  (setq pbuilder-log-view-build-result-alist
        (let* (new-data current-is-okay)
          (dolist (entry pbuilder-log-view-build-result-alist)
            (setq current-is-okay t)
            (if (get-buffer (cadr entry))
                (dolist (new-data-element new-data)
                  (if (string= (cadr new-data-element) (cadr entry))
                      (setq current-is-okay nil))
                  (if (string= (car new-data-element) (car entry))
                      (setq current-is-okay nil)))
              (setq current-is-okay nil))
            (if current-is-okay
                (add-to-list 'new-data entry)))
          (reverse new-data))))

(defun pbuilder-log-view-internal-view-one-log (result path ppath request)
  "View one logfile from buffer.

Requires a newish htmlize.el
RESULT is the resulting value
PATH is relative path from the published path
PPATH is the published path
REQUEST is the request data."
  (let* (logname matching-assoc nowlist charset)
    (string-match "/\\?\\(.+\\).html$" path)
    (setq logname (match-string 1 path))
    (setq nowlist (assoc logname pbuilder-log-view-build-result-alist))
    (if nowlist
        (save-window-excursion
          (if (get-buffer (cadr nowlist))
              (progn
                (let* ((htmlize-major-mode nil))
                  (set-buffer (htmlize-buffer-noninteractive (cadr nowlist))))
                (setq charset (detect-mime-charset-region (point-min)(point-max)))
                (elserv-set-result-header
                 result
                 (list 'content-type (concat "text/html; charset=" (symbol-name charset))))
                (elserv-set-result-body result
                                        (encode-mime-charset-string (buffer-string) charset))
                (kill-buffer (current-buffer)))
            (elserv-set-result-header result (list 'content-type (concat "text/plain")))
            (elserv-set-result-body result "404?")))
      (elserv-set-result-header result (list 'content-type (concat "text/plain")))
      (elserv-set-result-body result "404p"))))

;; some code sampled from remote.el from elserv sources.
(defun pbuilder-log-view-internal-function (result path ppath request)
  "Elserv publish function for pbuilder logs.
RESULT, PATH, PPATH and REQUEST are arguments

This page presents the list of build logs available from this Emacs session"
  (pbuilder-log-view-lock-mutex
   pbuilder-log-view-build-result-alist-mutex
   (pbuilder-log-view-internal-garbage-collect-log))
  (save-window-excursion
    (with-temp-buffer
      (elserv-set-result-header result
                                '(content-type "text/html"))
      (insert (concat "
<html>
<head>
<title>List of builds</title>
<style type=\"text/css\">"
                      pbuilder-log-view-css
                      "</style>
</head>
<body>
<h1 class=\"title\">List of builds done in the emacs session</h1>
<div class=\"listing\"><ul class=\"listing\">
"))
      (pbuilder-log-view-lock-mutex
       pbuilder-log-view-build-result-alist-mutex
       (dolist (nowlist pbuilder-log-view-build-result-alist)
         (if (get-buffer (cadr nowlist))
             (let* ((running-status
                     (if (caddr nowlist) (symbol-name (process-status (caddr nowlist)))
                       "nil")))
               (insert (concat "<li class=\"package\"><a class=\"package\" href=\""
                               pbuilder-log-view-web-basepath "query.cgi/?" (car nowlist) ".html\">"
                               (car nowlist)
                               "</a> <span class=\"status\">"
                               running-status
                               (if (string= running-status "exit")
                                   (if (= (process-exit-status (caddr nowlist)) 0)
                                       ": <span class=\"buildsuccess\">Successful build</span>"
                                     ": <span class=\"buildfail\">Build failure</span>")
                                 "")
                               "</span></li>\n")))))
       (insert (concat "</ul></div><p>Last updated:"
                       (current-time-string)
                       "</p></body>\n"))
       (elserv-set-result-body
        result
        (buffer-string))))))

(defcustom pbuilder-log-view-css "
BODY{
color: #ffeeee;
background-color: #000055;
}
h1.title{
   margin-top: 0em;
   border-color: #99c;
   border-width: 0px 9px 4px 0px;
   border-style: solid;
}
div.listing{
   margin-top: 0em;
   border-color: #99c;
   border-width: 0px 0px 4px 9px;
   border-style: solid;
}
li.package{
}
a:link {
  color: #ffccff;
}
a:active {
  color: #eeeeee;
}
a:hover {
  color: #ffffff;
  background-color: #5555ff;
}
a:visited {
  color: #ddeedd;
}
span.status{
  color: #ffffff;
  background-color: #000000;
}
span.buildfail{
  color: #ff3300;
  background-color: #000000;
}
span.buildsuccess{
  color: #00aaff;
  background-color: #000000;
}
" "*Css-string to be added to pbuilder log listing view html page.
h1.title
div.listing
ul.listing
li.package
a.package
span.status
span.buildfail
span.buildsuccess"
:type 'text
:group 'pbuilder-log-view)

(defun pbuilder-log-view-elserv ()
  "Run a elserv session with log view.

Running this requires elserv.  Use elserv, and do `elserv-start' before invoking this command."
  (interactive)
  (require 'elserv)
  (require 'htmlize)
  (elserv-publish (elserv-find-process)
                  pbuilder-log-view-web-basepath
                  :function 'pbuilder-log-view-internal-function
                  :description "Build log listing"
                  )
  (elserv-publish (elserv-find-process)
                  (concat pbuilder-log-view-web-basepath "query.cgi")
                  :function 'pbuilder-log-view-internal-view-one-log
                  :description "Build log database query"))


(provide 'pbuilder-log-view-mode)


