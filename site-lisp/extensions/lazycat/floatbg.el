;;; floatbg.el --- slowly modify background color

;; Copyright (C) 2001 John Paul Wallington

;; Author:  John Paul Wallington <jpw@shootybangbang.com>
;; Created: 07 Nov 2001
;; Version: 0.5, 11 Nov 2001
;; Keywords: background frames faces

;; This file isn't part of Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; Modifies backgound color by moving through an hsv color model, like
;; floatbg for X-Windows by Jan Rekers.

;; Installation:
;; Put floatbg.el somewhere in your load-path.
;; Put the following two lines in your .emacs file:
;; (require 'floatbg)
;; (floatbg-mode t)


;;; Code:


(defgroup floatbg nil
  "Slowly modify background color by moving through an HSV color model."
  :tag "Floating Background"
  :group 'frames
  :prefix "floatbg-")


(defcustom floatbg-mode nil
  "Toggle `floatbg-mode' on/off."
  :type 'boolean
  :tag "Toggle floatbg-mode on/off."
  :initialize 'custom-initialize-default
  :set (lambda (symbol value) (floatbg-mode value))
  :require 'floatbg
  :group 'floatbg)


(defcustom floatbg-delay 15
  "* Delay in seconds before changing color."
  :type 'number
  :group 'floatbg)


(defcustom floatbg-increment 1
  "* Size of increment of Hue in degrees when changing color."
  :type 'number
  :group 'floatbg)


(defcustom floatbg-initial-hue t
  "* Initial value of Hue (in HSV model) in degrees."
  :type '(choice integer
                 (const :tag "Derived from time of day" t)
                 (const :tag "Random" nil))
  :group 'floatbg)


(defun floatbg-set-val (symbol value)
  (if (and (numberp value)
           (< 0.0 value)
           (< value 1.0))
      (set-default symbol value)
    (error "please set %s to more than 0.0 and less than 1.0"
           (symbol-name symbol))))


(defcustom floatbg-initial-val 0.88
  "* Initial value of Value (in HSV model); should be > 0.0 and < 1.0."
  :type '(number :tag "Number more than 0.0 and less than 1.0")
  :set 'floatbg-set-val
  :group 'floatbg)


(defvar floatbg-smid 0.375)
(defvar floatbg-svar 0.125)
(defvar floatbg-sfinhf 0.25)


(defun floatbg-set-sinus-shape (symbol value)
  (let ((smid (car value))
        (svar (car (cdr value)))
        (sfinhf (car (nthcdr 2 value))))
    (unless (null value)
      (if (and (>= 1 (- smid svar))
               (>= 1 (+ smid svar))
               (<= 0 (- smid svar))
               (<= 0 (+ smid svar)))
          (setq floatbg-smid smid
                floatbg-svar svar
                floatbg-sfinhf sfinhf)
        (error "Invalid parameters.")))))


(defcustom floatbg-sinus-shape nil
  "* The sinus shape.
 Unquoted list containing smid, svar and sfinhf parameters.
 The default is (0.375 0.125 0.25).
 smid + svar and smid - svar should fall between 0 and 1."
  :type '(choice (const :tag "Default" nil)
                 (sexp :tag "Specify List"))
  :set 'floatbg-set-sinus-shape
  :group 'floatbg)


(defcustom floatbg-reset-on-toggle nil
  "* Reset colors to initial values when toggling `floatbg-mode'?"
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'floatbg)


(defvar floatbg-timer nil
  "Timer handle for floatbg mode.")


(defun floatbg-initial-hue ()
  (if (equal floatbg-initial-hue t)
      (* (1+ (car (nthcdr 2 (decode-time)))) 15)
    (or floatbg-initial-hue (random 360))))


(defvar floatbg-hue (floatbg-initial-hue))
(defvar floatbg-sat)
(defvar floatbg-val floatbg-initial-val)


;;;###autoload
(defun floatbg-mode (&optional arg)
  "Toggle floatbg mode"
  (interactive "P")
  (if floatbg-timer (cancel-timer floatbg-timer))
  (when (setq floatbg-mode
              (if (null arg)
                  (not floatbg-mode)
                (> (prefix-numeric-value arg) 0)))
    (if floatbg-reset-on-toggle
        (floatbg-reset-initial-values))
    (setq floatbg-timer
          (run-at-time 1 floatbg-delay 'floatbg-change)))
  (message "floatbg-mode now %s" (if floatbg-mode "on" "off")))


(defun floatbg-change ()
  "Change background color, imperceptibly."
  (setq floatbg-hue (mod (+ floatbg-hue floatbg-increment) 360)
        floatbg-sat (- floatbg-smid
                       (* floatbg-svar
                          (sin (* (/ pi 180) floatbg-sfinhf floatbg-hue)))))
  (let ((background
         (floatbg-hsv-to-rgb-string floatbg-hue floatbg-sat floatbg-val))
        (frames (frame-list)))
    (while frames
      (modify-frame-parameters (car frames)
                               (list (cons 'background-color background)))
      (setq frames (cdr frames)))
    (set-face-background 'default background)))


(defun floatbg-hsv-to-rgb-string (h s v)
  "Convert color in HSV values to RGB string."
  (setq h (degrees-to-radians h))
  (let (r g b)
    (if (zerop s)
        (setq r v g v b v)
      (let* ((h (/ (if (>= h (* 2 pi)) 0.0 h)
                   (/ pi 3)))
             (i (truncate h))
             (f (- h i)))
        (let ((p (* v (- 1.0 s)))
              (q (* v (- 1.0 (* s f))))
              (z (* v (- 1.0 (* s (- 1.0 f))))))
          (cond ((eq i 0) (setq r v g z b p))
                ((eq i 1) (setq r q g v b p))
                ((eq i 2) (setq r p g v b z))
                ((eq i 3) (setq r p g q b v))
                ((eq i 4) (setq r z g p b v))
                ((eq i 5) (setq r v g p b q))))))
    (format "#%.2X%.2X%.2X" (* r 255) (* g 255) (* b 255))))


(defun floatbg-reset-initial-values ()
  "Reset floatbg colors to initial values."
  (interactive)
  (setq floatbg-hue (floatbg-initial-hue)
        floatbg-val floatbg-initial-val))


(provide 'floatbg)
;;; floatbg.el ends here
