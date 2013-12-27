;;; mldonkey-header-line.el --- Part of the Emacs Interface to MLDonkey

;; Copyright (c) 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.0.4

;; mldonkey-header-line.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey-header-line.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file is part if the mldonkey.el package.
;;
;; MLDonkey is a multi-networks peer-to-peer client.
;; See <http://www.nongnu.org/mldonkey/> for details.
;;
;; mldonkey.el connects to the telnet interface of the MlDonkey core
;; and uses a lot of regexps to parse the output.  So it might not work
;; with future versions of MlDonkey.

;;; Documentation:
;;
;;  This module automatically sets the header line to show some statistics.



;;; Code:

(require 'mldonkey-vd)
(require 'mldonkey-commands)

;;;; Faces:

(defface mldonkey-header-line-mldonkey-face
  '((((type tty) (class color))
     (:foreground "green"))
    (((type tty) (class mono))
     (:weight bold))
    (((class color) (background dark))
      (:foreground "chartreuse3" :inherit variable-pitch))
    (((class color) (background light))
      (:foreground "green3" :inherit variable-pitch)))
  "Face for \"MLDonkey\" text in the header line."
  :group 'mldonkey-faces)

(defface mldonkey-header-line-downloaded-face
  '((((type tty) (class color))
     (:foreground "white" :weight bold))
    (((type tty) (class mono))
     (:weight bold :inherit 'mode-line))
    (((class color) (background dark))
      (:foreground "white" :inherit variable-pitch))
    (((class color) (background light))
      (:foreground "black" :inherit variable-pitch)))
  "Face for the header lines \"downloaded\" part."
  :group 'mldonkey-faces)

(defface mldonkey-header-line-bw-up-face
  '((((type tty) (class color))
     (:foreground "blue" :weight bold))
    (((type tty) (class mono))
     (:weight bold :inherit 'mode-line))
    (((class color) (background dark))
     (:foreground "deep sky blue" :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "medium slate blue" :inherit variable-pitch)))
  "Face for the header lines upload stats part."
  :group 'mldonkey-faces)

(defface mldonkey-header-line-bw-down-face
  '((((type tty) (class color))
     (:foreground "green" :weight bold))
    (((type tty) (class mono))
     (:weight bold :inherit 'mode-line))
    (((class color) (background dark))
     (:foreground "chartreuse1" :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "green" :inherit variable-pitch)))
  "Face for the header lines download stats part."
  :group 'mldonkey-faces)


(defun mldonkey-header-line-motd-hook ()

  "Function to add to `mldonkey-motd-hook'."

  (mldonkey-header-line-update))

(defun mldonkey-header-line-vd-hook ()

  "Function to add to 'mldonkey-vd-hook'."

  (mldonkey-bw-stats t)
  (mldonkey-header-line-update))


(add-hook 'mldonkey-vd-hook 'mldonkey-header-line-vd-hook)
(add-hook 'mldonkey-motd-hook 'mldonkey-header-line-motd-hook)
(add-hook 'mldonkey-bw-stats-hook 'mldonkey-header-line-update)

(defun mldonkey-header-line-update ()

  "Update the header line in a MlDonkey buffer."

  (when (fboundp 'propertize)
    (with-current-buffer (mldonkey-get-mldonkey-buffer)
      (if (and mldonkey-vd-num-downloading mldonkey-vd-num-finished)
          (progn
            (setq header-line-format
                  (list
                   (propertize "  MlDonkey"
                               'face 'mldonkey-header-line-mldonkey-face)
                   "\t"
                   (propertize (concat "Downloaded " (number-to-string
                                                      mldonkey-vd-num-finished)
                                       "/" (number-to-string
                                            mldonkey-vd-num-downloading))
                               'face 'mldonkey-header-line-downloaded-face)))
            (when (and mldonkey-bw-stats-down mldonkey-bw-stats-up)
              (setq header-line-format
                    (append header-line-format
                            (list "\t"
                                  (propertize
                                   mldonkey-bw-stats-down
                                   'face 'mldonkey-header-line-bw-down-face)
                                  " "
                                  (propertize
                                   mldonkey-bw-stats-up
                                   'face 'mldonkey-header-line-bw-up-face))))))
        (setq header-line-format
              (propertize "  Welcome to MlDonkey"
                          'face 'mldonkey-header-line-mldonkey-face)))
      (force-mode-line-update))))


(provide 'mldonkey-header-line)

;;; mldonkey-console.el ends here