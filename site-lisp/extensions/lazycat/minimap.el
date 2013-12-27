;;; minimap.el --- Minimap sidebar for Emacs

;; Copyright (C) 2009  David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords:
;; Version: 0.2

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is an implementation of a minimap sidebar, e.g., a
;; smaller display of the current buffer on the left side. It
;; highlights the currently shown region and updates its position
;; automatically.

;; Usage: Use 'M-x minimap-create' in a buffer. Note that this is
;; currently more a 'proof of concept'. Patches are welcome.

;;; TODO:

;; * Make sidebar permanently visible. This requires something like a
;;   'window group' feature in Emacs, which is currently being worked on.
;; * Make stuff customizable
;; * Deal with images.
;; * Fix some display quirks, especially the rough scrolling.
;; * Automatically deal with light/dark backgrounds.

;;; User variables:

(defvar minimap-font '(:family
		       "Bitstream Vera Sans Mono"
		       :height 40)
  "Font related attributes for minimap buffer.")

(defvar minimap-active-overlay-background '(:background "gray30")
  "Background color for active region.")

(defvar minimap-mouse-active-cccoverlay-background '(:background "gray60")
  "Background color for active region with mouse.")

(defvar minimap-width-fraction 0.2
  "Fraction of width which should be used for minimap sidebar.")

(defvar minimap-buffer-name-prefix "*MINIMAP* "
  "Prefix for buffer names of minimap sidebar.")

(defvar minimap-update-delay 0.5
  "Delay in seconds after which sidebar gets updated.")

(defvar minimap-always-recenter t
  "Whether minimap sidebar should be recentered after every point movement.")

;;; Internal variables

(make-variable-buffer-local 'minimap-start)
(make-variable-buffer-local 'minimap-end)
(make-variable-buffer-local 'minimap-active-overlay)

;;; Minimap creation

(defun minimap-create ()
  "Create a minimap sidebar for the current window."
  (interactive)
  (let ((bufname (concat minimap-buffer-name-prefix
			 (buffer-name (current-buffer))))
	(new-win (split-window-horizontally (round (* (window-width) minimap-width-fraction))))
	(start (window-start))
	(end (window-end))
	ov indbuf)
    (if (get-buffer bufname)
	(switch-to-buffer bufname t)
      (setq indbuf (make-indirect-buffer (current-buffer)
					 (concat minimap-buffer-name-prefix
						 (buffer-name (current-buffer)))
					 t))
      (switch-to-buffer indbuf)
      (setq ov (make-overlay (point-min) (point-max) nil t t))
      (overlay-put ov 'face minimap-font)
      (setq minimap-start start
	    minimap-end end
	    minimap-active-overlay (make-overlay start end)
	    line-spacing 0)
      (overlay-put minimap-active-overlay 'face minimap-active-overlay-background)
      (minimap-mode 1)
      (setq buffer-read-only t))
    (other-window 1)))

;;; Minimap update

(defun minimap-update ()
  "Update minimap sidebar.
This is meant to be called from the idle-timer or the post command hook."
    (save-excursion
      (let ((win (get-buffer-window (concat minimap-buffer-name-prefix
					    (buffer-name (current-buffer)))))
	    start end pt ov)
	(when win
	  (setq start (window-start)
		end (window-end)
		pt (point)
		ov)
	  (with-selected-window win
	    (unless (and (= minimap-start start)
			 (= minimap-end end))
	      (move-overlay minimap-active-overlay start end)
	      (setq minimap-start start
		    minimap-end end))
	    (goto-char pt)
	    (when minimap-always-recenter
	      (recenter (round (/ (window-height) 2)))))))))

;;; Minimap minor mode

(defvar minimap-mode-map (make-sparse-keymap)
  "Keymap used by `minimap-mode'.")

(define-key minimap-mode-map [down-mouse-1] 'minimap-move-overlay-mouse)

(define-minor-mode minimap-mode
  "Minor mode for minimap sidebar."
  nil "minimap" minimap-mode-map)

(defun minimap-move-overlay-mouse (start-event)
  "Move overlay by tracking mouse movement."
  (interactive "e")
  (setq minimap-got-mouse-up nil)
  (let* ((_ (mouse-set-point start-event))
	 (echo-keystrokes 0)
	 (end-posn (event-end start-event))
	 (start-point (posn-point end-posn))
	 (make-cursor-line-fully-visible nil)
	 pt)
    (move-overlay minimap-active-overlay start-point minimap-end)
    (track-mouse
      (minimap-set-overlay start-point)
      (while (and
	      (consp (setq ev (read-event)))
	      (eq (car ev) 'mouse-movement))
	(setq pt (posn-point (event-start ev)))
	(when (numberp pt)
	  (minimap-set-overlay pt)
	  )))
    (select-window (get-buffer-window (buffer-base-buffer)))
    (minimap-update)
    ))

(defun minimap-set-overlay (pt)
  "Set overlay position, with PT being the middle."
  (goto-char pt)
  (let* ((ovstartline (line-number-at-pos minimap-start))
	 (ovendline (line-number-at-pos minimap-end))
	 (ovheight (round (/ (- ovendline ovstartline) 2)))
	 (line (line-number-at-pos))
	 (winstart (window-start))
	 (winend (window-end))
	 newstart newend)
    (setq pt (point-at-bol))
    (setq newstart (minimap-line-to-pos (- line ovheight)))
    (while (< newstart winstart)
      (scroll-down 5)
      (redisplay t)
      (setq winstart (window-start)))
    (with-selected-window (get-buffer-window (buffer-base-buffer))
      (set-window-start nil newstart)
      (setq newend (window-end)))
    (while (> newend winend)
      (scroll-up 5)
      (redisplay t)
      (setq winend (window-end)))
    (move-overlay minimap-active-overlay newstart newend)
))

(defun minimap-line-to-pos (line)
  "Returns point position of line number LINE."
  (save-excursion
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))
    (point)))

(run-with-idle-timer minimap-update-delay t 'minimap-update)

(provide 'minimap)

;;; minimap.el ends here
