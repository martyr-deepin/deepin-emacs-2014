;;; mldonkey-console.el --- Part of the Emacs Interface to MLDonkey

;; Copyright (c) 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.0.4

;; mldonkey-console.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; mldonkey-console.el is distributed in the hope that it will be useful,
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

;;; Change log
;;
;; 0.0.3a:
;;   * made `comint-preoutput-filter-functions' buffer local

;;; Documentation:
;;
;; This module implements a comint buffer direct interaction with the
;; MlDonkey core.  Just put (require 'mldonkey-console) in you ~/.emacs.
;; To start the console use the function `mldonkey-console' or hit "c"
;; in the *MlDonkey* buffer.
;;
;; Example configuration:
;;
;; ;; This is the default
;; (setq mldonkey-console-use-color t)





;;; Code:

(require 'mldonkey)
(require 'comint)
(require 'ansi-color)

(defcustom mldonkey-console-use-color t
  "Use colors in the MlDonkey console."
  :group 'mldonkey
  :type 'boolean)


(defconst mldonkey-console-buffer-name "MlDonkey Console"
  "Name of the MlDonkey console buffer.")

(defconst mldonkey-console-buffer-regexp "\\*MlDonkey Console\\*"
  "Regexp used to find the MlDonkey console buffer.")

(defvar mldonkey-console-hook nil
  "*Hook called after creating a mldonkey=console buffer.")


(defun mldonkey-console (&optional no-pop)

  "Open a comint buffer with a connection to the mldonkey-core."

  (interactive)

  (let ((buffer (mldonkey-console-get-buffer)) (run-proc t))
    (if buffer
        (progn
          (pop-to-buffer buffer)
          (goto-char (point-max))
          (if (not (comint-check-proc buffer))
              (mldonkey-console-make-comint)))
      (setq buffer (mldonkey-console-make-comint))
      (unless no-pop
        (pop-to-buffer buffer)))))


(defun mldonkey-console-get-buffer ()

  "Get the mldonkey-console buffer if there or nil if there is none"

  (mldonkey-find-buffer "\\*MlDonkey Console\\*" 'comint-mode))


(defun mldonkey-console-set-colors ()

  "Set the variable `ansi-color-for-comint-mode'.

This function is used in `comint-exec-hooks'."

  (with-current-buffer (mldonkey-console-get-buffer)
    (setq comint-process-echoes t)
    (make-variable-buffer-local 'ansi-color-for-comint-mode)
    (if mldonkey-console-use-color
        (setq ansi-color-for-comint-mode t)
      (setq ansi-color-for-comint-mode 'filter))))


(defun mldonkey-console-filter-output (out)

  "Filter the first line (the echoed command) from the output."

  out)
;;   (if (string-match "^.+?$" out)
;;       (substring out (match-end 0))))


(defun mldonkey-console-make-comint ()

  "Makes the mldonkey-console buffer using `make-comint'."

  (let ((buffer))
    (add-hook 'comint-exec-hook 'mldonkey-console-set-colors)
    (setq buffer (make-comint mldonkey-console-buffer-name
                              (cons mldonkey-host mldonkey-port)))
    (remove-hook 'comint-exec-hook 'mldonkey-console-set-colors)
    (with-current-buffer buffer
      (make-variable-buffer-local 'comint-preoutput-filter-functions)
      (add-hook 'comint-preoutput-filter-functions
                'mldonkey-console-filter-output)
      (run-hooks 'mldonkey-console-hook))
    buffer))


(defun mldonkey-console-send-command (str &optional pop-to)

  (let ((buffer (mldonkey-console-get-buffer)))
    (unless buffer
      (setq buffer (mldonkey-console-make-comint)))
    (with-current-buffer buffer
      (let ((proc (get-buffer-process buffer)))
        (unless proc
          (setq buffer (mldonkey-console-make-comint)))
        (comint-send-string proc (concat str "\n"))))
    (if pop-to
        (pop-to-buffer buffer))))


(define-key mldonkey-mode-map "c" 'mldonkey-console)


(provide 'mldonkey-console)

;;; mldonkey-console.el ends here