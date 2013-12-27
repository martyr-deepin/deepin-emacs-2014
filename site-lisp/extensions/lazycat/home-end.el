;;; home-end.el --- Alternative Home and End commands.
;; Copyright 1996 Kai Grossjohann and Toby Speight
;; Copyright 2002 Toby Speight

;; home-end.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.


;;; Commentary:
;;
;; Some useful bindings for Home and End keys:
;; Hit the key once to go to the beginning/end of a line,
;; hit it twice in a row to go to the beginning/end of the window,
;; three times in a row goes to the beiginning/end of the buffer.
;; N.B. there is no timeout involved.
;;
;; To use:
;;  (global-set-key [end]  'home-end-end)
;;  (global-set-key [home] 'home-end-home)


;;; History:
;;
;; Kai Grossjohann <grossjohann@ls6.informatik.uni-dortmund.de>
;; 29 Jul 96:
;; Posted to Usenet.
;;
;; Modified by Toby Speight <tms@ansa.co.uk>
;; 1996-11-14:
;; Ensure that mark is set only when moving to beginning of window,
;; and is not set again when moving to beginning of buffer.
;;
;; Modified by Toby Speight <streapadair@gmx.net>>
;; 2002-07-12:
;; Added comments and license terms (with Kai's agreement).
;; Added autoload cookies.

(defvar home-end-marker)

;;;###autoload
(defun home-end-home (&optional arg)
  "Go to beginning of line/window/buffer.
First hitting key goes to beginning of line, second in a row goes to
beginning of window, third in a row goes to beginning of buffer."
  (interactive "P")
  (if arg
      (beginning-of-buffer arg)
    (let* ((keys (recent-keys))
           (len (length keys))
           (key1 (if (> len 0) (elt keys (- len 1)) nil))
           (key2 (if (> len 1) (elt keys (- len 2)) nil))
           (key3 (if (> len 2) (elt keys (- len 3)) nil))
           (key-equal-1 (equal key1 key2))
           (key-equal-2 (and key-equal-1 (equal key2 key3))))
      (cond (key-equal-2 (goto-char (point-min)))
            (key-equal-1 (push-mark home-end-marker)
                         (move-to-window-line 0))
            (t (setq home-end-marker (copy-marker (point)))
               (beginning-of-line))))))

;;;###autoload
(defun home-end-end (&optional arg)
  "Go to end of line/window/buffer.
First hitting key goes to end of line, second in a row goes to end
of window, third in a row goes to end of buffer."
  (interactive "P")
  (if arg
      (beginning-of-buffer arg)
    (let* ((keys (recent-keys))
           (len (length keys))
           (key1 (if (> len 0) (elt keys (- len 1)) nil))
           (key2 (if (> len 1) (elt keys (- len 2)) nil))
           (key3 (if (> len 2) (elt keys (- len 3)) nil))
           (key-equal-1 (equal key1 key2))
           (key-equal-2 (and key-equal-1 (equal key2 key3))))
      (cond (key-equal-2 (goto-char (point-max)))
            (key-equal-1 (push-mark home-end-marker)
                         (move-to-window-line -1)
                         (end-of-line))
            (t (setq home-end-marker (copy-marker (point)))
               (end-of-line))))))

(provide 'home-end)
