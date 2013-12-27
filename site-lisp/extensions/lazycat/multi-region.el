;;; multi-region.el --- Mapping commands over multiple active regions.

;; This file is NOT part of Emacs.

;; Copyright (C) 2004, 2005 Lawrence Mitchell <wence@gmx.li>
;; Filename: multi-region.el
;; Version: 1.1
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-04-15
;; URL: http://purl.org/NET/wence/multi-region.el

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; Marking, and mapping commands over, multiple regions.
;;
;; Available commands:
;; `multi-region-mark-region'     -- Mark the current region as a
;;                                   multi-region.
;; `multi-region-unmark-region'   -- Remove the current multi-region
;;                                   surrounding `point'.
;; `multi-region-unmark-regions'  -- Remove all multi-regions in the
;;                                   current buffer.
;; `multi-region-execute-command' -- Map a command over all active
;;                                   multi-regions in the current
;;                                   buffer.
;;
;; A suggested keymap is available as `multi-region-map', to use
;; the keybindings, you could do something like:
;; (define-key global-map (kbd "C-M-m") multi-region-map)

;;; History:
;; Inspired by <URL:
;; http://www.emacswiki.org/cgi-bin/wiki/ApplyFunctionOnMultipleRegions>

;;; Code:

(when (featurep 'xemacs)
  (require 'overlay))

(defface multi-region-face
  '((((background light))
     (:background "lightblue"))
    (((background dark))
     (:background "darkblue"))
    (t (:background "lightblue")))
  "Face to highlight multi-regions.")

(defvar multi-region-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'multi-region-mark-region)
    (define-key map "r" 'multi-region-unmark-regions)
    (define-key map "d" 'multi-region-unmark-region)
    (define-key map "x" 'multi-region-execute-command)
    map)
  "Keymap for multi-region commands.")

(defvar multi-region-overlays nil)
(make-variable-buffer-local 'multi-region-overlays)

(defun multi-region-overlay-p (overlays)
  "Return non-nil if OVERLAYS contains a multi-region overlay.

The overlay is returned as a non-nil value."
  (catch 'done
    (dolist (ov overlays)
      (when (overlay-get ov 'multi-region)
        (throw 'done ov)))))

(defun multi-region-mark-region (beg end)
  "Add a multi-region.

The region between BEG and END is given a multi-region overlay."
  (interactive "r")
  (let ((ov (multi-region-overlay-p (overlays-at beg))))
    (unless (and ov (= (overlay-end ov) end))
      (setq ov (make-overlay beg end))
      (overlay-put ov 'multi-region t)
      (overlay-put ov 'face 'multi-region-face)
      (push ov multi-region-overlays)))
  (when (and (boundp 'transient-mark-mode)
             transient-mark-mode)
    (deactivate-mark)))

(defun multi-region-unmark-regions ()
  "Remove all multi-regions in the current buffer."
  (interactive)
  (mapc #'delete-overlay multi-region-overlays)
  (setq multi-region-overlays nil))

(defun multi-region-unmark-region ()
  "Remove the multi-region around `point'."
  (interactive)
  (let ((ov (multi-region-overlay-p (overlays-at (point)))))
    (unless ov
      (error "No multi-region around point"))
    (setq multi-region-overlays (delete ov multi-region-overlays))
    (delete-overlay ov)))

(defun multi-region-execute-command (&optional arg cmd)
  "Perform a command on all active multi-regions.

ARG gets passed through as `current-prefix-arg' to the command
called.  If CMD is non-nil, call that, rather than prompting for
one."
  (interactive "P")
  (setq cmd (or cmd (key-binding (read-key-sequence " " t))))
  (when (eq cmd 'execute-extended-command)
    (setq cmd (read-command "M-x ")))
  (setq current-prefix-arg arg)
  (dolist (ov (reverse multi-region-overlays))
    (let ((start (overlay-start ov))
          (end (overlay-end ov)))
      (save-excursion
        ;; Ensure that we only operate on the marked region.  The
        ;; other alternative, to narrow to the marked region, looks
        ;; ugly when performing commands like ispell-region, however,
        ;; it's failsafe, whereas this version assumes that package
        ;; authors respect transient-mark-mode.
        (let ((transient-mark-mode t)
              (zmacs-regions t))
          (push-mark start nil t)
          (goto-char end)
          (command-execute cmd)))))
  (when (fboundp 'deactivate-mark)
    (deactivate-mark))
  (when (fboundp 'zmacs-deactivate-region)
    (zmacs-deactivate-region))
  (multi-region-unmark-regions))

(provide 'multi-region)

;;; multi-region.el ends here
