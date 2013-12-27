;;; dpkg-dev-el.el --- startup file for the debian-el package

;;; Commentary:
;;
;; This file is loaded from /etc/emacs/site-start.d/50dpkg-dev-el.el

;;; History:
;;
;; 2003-11-03 - Peter Galbraith
;;  - Created.

;;; Code:

(defgroup dpkg-dev-el nil
  "Emacs helpers specific to Debian development."
  :group 'convenience)

(require 'dpkg-dev-el-loaddefs)

;; debian-bts-control
(defgroup debian-bts-control nil
  "Create messages for Debian BTS control interface"
  :group 'debian-bug
  ;;:link '(custom-manual "(dpkg-dev-el)debian-bts-control")
  :load 'debian-bts-control
  :group 'dpkg-dev-el)

;; debian-changelog-mode
(defgroup debian-changelog nil "Debian changelog maintenance"
  :group 'tools
  :prefix "debian-changelog-"
  ;;:link '(custom-manual "(dpkg-dev-el)debian-changelog-mode")
  :load 'debian-changelog-mode
  :group 'dpkg-dev-el)

;; debian-control-mode
(defgroup debian-control nil "Debian control file maintenance"
  :link '(url-link "http://cvs.verbum.org/debian/debian-control-mode")
  :group 'tools
  ;;:link '(custom-manual "(dpkg-dev-el)debian-control-mode")
  :load 'debian-control-mode
  :group 'dpkg-dev-el)

;; debian-copyright
(defgroup debian-copyright nil "Debian copyright mode"
  :group 'tools
  :prefix "debian-copyright-"
  ;;:link '(custom-manual "(dpkg-dev-el)debian-copyright")
  :load 'debian-copyright
  :group 'dpkg-dev-el)

;; readme-debian
(defgroup readme-debian nil "Readme Debian (mode)"
  :group 'tools
  :prefix "readme-debian-"
  ;;:link '(custom-manual "(dpkg-dev-el)readme-debian")
  :load 'readme-debian
  :group 'dpkg-dev-el)




;; other useful automode
(add-to-list 'auto-mode-alist
             '("/debian/[^/]*emacsen-startup\\'" . emacs-lisp-mode))
;; Closes #490292
(add-to-list 'auto-mode-alist '("README.source" . readme-debian-mode))

(when (member 'utf-8 (coding-system-list))
  ;; default to utf-8 for debian changelog files
  (modify-coding-system-alist 'file "/changelog\\.Debian\\'" 'utf-8)
  (modify-coding-system-alist 'file "/debian/control\\'" 'utf-8)

;;; (modify-coding-system-alist 'file "/debian/changelog\\'" 'utf-8)
;;; -
;;; Kevin Ryde <user42@zip.com.au> (Closes: #587921)
;;;
;;; Instead use this for dh_installchangelog debian/packagename.changelog
;;; files too.  See http://bugs.debian.org/457047 by Trent W. Buck
;;; But not [:lower:][:digit:] since those forms are not available in xemacs21.
;;; xemacs21 can have utf-8 at startup if you use mule-ucs with
;;; DEB_MULEUCS_UNICODE=yes
  (modify-coding-system-alist 'file "/debian/\\([a-z0-9.+-]+\\.\\)?changelog\\'" 'utf-8)

  ;; Handle Debian native package, from Kevin Ryde in bug #317597 and #416218
  (defun debian-changelog-coding-system (args)
    "Return the coding system for a /usr/share/doc/[package]/changelog file.
If [package] is a debian native (no separate changelog.Debian) then answer
`utf-8', otherwise remove ourselves from `file-coding-system-alist' and see
what other rules say."
    (let ((filename (if (consp (cadr args))
                        (car (cadr args)) ;; ("filename" . buffer) in emacs 22
                      (cadr args)))       ;; "filename" in emacs 21
          (dirname  (file-name-directory filename)))
      (if (file-exists-p (concat dirname "changelog.Debian.gz"))
          (let ((file-coding-system-alist
                 (remove '("/usr/share/doc/[^/]+/changelog\\'"
                           . debian-changelog-coding-system)
                         file-coding-system-alist)))
            (apply 'find-operation-coding-system args))
        'utf-8))))


(provide 'dpkg-dev-el)

;;; dpkg-dev-el.el ends here
