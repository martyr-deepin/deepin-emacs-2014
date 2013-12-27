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

(require 'devscripts)
(require 'pbuilder-log-view-mode)

(defgroup pbuilder nil "PBuilder mode"
  :group 'tools
  :prefix "pbuilder-mode-")

(defcustom pbuilder-path "/usr/sbin/pbuilder"
  "*Path to pbuilder."
  :group 'pbuilder
  :type 'file)
(defcustom pbuilder-user-mode-linux-path "/usr/bin/pbuilder-user-mode-linux" "*Path to pbuilder-user-mode-linux."
  :group 'pbuilder
  :type 'file)
(defcustom pdebuild-path "/usr/bin/pdebuild" "*Path to pdebuild."
  :group 'pbuilder
  :type 'file)
(defcustom pdebuild-user-mode-linux-path "/usr/bin/pdebuild-user-mode-linux" "*Path to pdebuild-user-mode-linux."
  :group 'pbuilder
  :type 'file)
(defcustom debuild-pbuilder-path "/usr/bin/debuild-pbuilder" "*Path to `debuild-pbuilder'."
  :group 'pbuilder
  :type 'file)
(defconst pbuilder-mode-version "$Id: pbuilder-mode.el,v 1.3 2007-07-14 09:26:05 dancer Exp $" "Version of pbuilder mode.")

(defun pdebuild ()
  "Run pdebuild in the current directory."
  (interactive)
  (let* ((pdebuild-buffer (concat "*pdebuild*" default-directory))
         (pdebuild-process (concat "pdebuild-process-" default-directory))
         (package-name (devscripts-internal-get-debian-package-name)))
    (switch-to-buffer pdebuild-buffer)
    (toggle-read-only 0)
    (kill-region (point-min) (point-max))
    (compilation-mode)
    (pbuilder-log-view-add
     package-name pdebuild-buffer
     (start-process pdebuild-process pdebuild-buffer pdebuild-path))))

(defun pdebuild-user-mode-linux ()
  "Run pdebuild-user-mode-linux in the current directory."
  (interactive)
  (let* ((pdebuild-buffer (concat "*pdebuild*" default-directory))
         (pdebuild-process (concat "pdebuild-process-" default-directory))
         (package-name (devscripts-internal-get-debian-package-name)))
    (switch-to-buffer pdebuild-buffer)
    (toggle-read-only 0)
    (kill-region (point-min) (point-max))
    (compilation-mode)
    (pbuilder-log-view-add
     package-name pdebuild-buffer
     (start-process pdebuild-process pdebuild-buffer pdebuild-user-mode-linux-path))
    (set-buffer-process-coding-system 'dos 'dos)))

(defun debuild-pbuilder ()
  "Run `debuild-pbuilder' in the current directory."
  (interactive)

  (let* ((pdebuild-name (concat "debuild-pbuilder" default-directory))
         (pdebuild-buffer (concat "*" pdebuild-name "*" ))
         (pdebuild-process (concat "debuild-pbuilder-process-" default-directory))
         (package-name (devscripts-internal-get-debian-package-name)))
    (switch-to-buffer pdebuild-buffer)
    (toggle-read-only 0)
    (kill-region (point-min) (point-max))
    (pbuilder-log-view-add
     package-name
     (apply 'make-comint pdebuild-name debuild-pbuilder-path nil
            debuild-option-list)
     (get-process pdebuild-name))))

(defun pbuilder-build (filename)
  "Run pbuilder build for a given FILENAME.
Uses `devscripts-mode-gain-root-command' as command to gain root."
  (interactive "f.dsc File name: ")
  (let* ((pbuilder-buffer (concat "*pbuilder-build*" filename))
         (pbuilder-process (concat "pbuilder-build-process-" filename)))
    (switch-to-buffer pbuilder-buffer)
    (toggle-read-only 0)
    (kill-region (point-min) (point-max))
    (compilation-mode)
    (insert "start compile\n")
    (pbuilder-log-view-add
     (file-name-sans-extension (file-name-nondirectory filename)) pbuilder-buffer
     (start-process pbuilder-process pbuilder-buffer devscripts-mode-gain-root-command pbuilder-path "build" (expand-file-name filename)))))

(defun pbuilder-user-mode-linux-build (filename)
  "Run pbuilder-user-mode-linux build for a given FILENAME. "
  (interactive "f.dsc File name: ")
  (let* ((pbuilder-buffer (concat "*pbuilder-uml-build*" filename))
         (pbuilder-process (concat "pbuilder-uml-build-process-" filename)))
    (switch-to-buffer pbuilder-buffer)
    (toggle-read-only 0)
    (kill-region (point-min) (point-max))
    (compilation-mode)
    (insert "start compile\n")
    (pbuilder-log-view-add
     (file-name-sans-extension (file-name-nondirectory filename)) pbuilder-buffer
     (start-process pbuilder-process pbuilder-buffer pbuilder-user-mode-linux-path "build" (expand-file-name filename)))
    (set-buffer-process-coding-system 'dos 'dos)))



(provide 'pbuilder-mode)
