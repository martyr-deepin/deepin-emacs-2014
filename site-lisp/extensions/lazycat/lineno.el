;;; lineno.el
;;;
;;; Alternate mode to display line numbers. While there are other
;;; functions that also do this, lineno is better suited for large
;;; files since it does not put line numbers throughout the buffer,
;;; but only puts them in the visible window. This way the size of the
;;; file does not matter, it is the window size which determines the
;;; update speed, and this is fast enough.
;;;
;;; It only works in file buffers so far, I looked briefly at adding
;;; it to shell buffers, but there are some update issues. There also
;;; seems to be some funny interaction with fill-paragraph which turns
;;; into an infinite loop, so you might want to avoid that function.
;;; (^G breaks it out)
;;;
;;; As with other line numbering functions, tabs work funny, so it is
;;; good if you can set your width to a tab width (I use 4) or replace
;;; with spaces.
;;;
;;; To install: put in path, (require 'lineno)
;;; To use: M-x lineno-minor-mode <RET> to toggle it.
;;;
;;; c 2007 Russell Young
;;; emacs@young-0.com
;;; Open source: anyone can use, distribute, or modify, but please
;;; leave in the attribution
;;; http://www.emacswiki.org/emacs/LineNumbers
(defface lineno-face
  '((t (:foreground "red" )
       (:foreground: "black" :background "white")))
  "Default face used for line numbers in lineno-minor-mode
Note: Making this face larger than the buffer face (for instance,
making it bold) messes up some movement functions, like 'M->' or
'M-v'. If you make sure the line number font is smaller than the
buffer font this won't be a problem."
  :group 'lineno)

(defcustom lineno-face 'lineno-face
  "Face to use to display line numbers
Note: Making this face larger than the buffer face (for instance,
making it bold) messes up some movement functions, like 'M->' or
'M-v'. If you make sure the line number font is smaller than the
buffer font this won't be a problem."
  :group 'lineno
  :type 'face)

(defvar lineno-overlays nil)
(make-variable-buffer-local 'lineno-overlays)
(defvar lineno-overlay nil)
(make-variable-buffer-local 'lineno-overlay)
(defvar lineno-update-from nil)
(make-variable-buffer-local 'lineno-update)
(defvar lineno-window-start nil)
(make-variable-buffer-local 'lineno-window-start)
(defvar lineno-lineno-width nil)
(make-variable-buffer-local 'lineno-lineno-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Painting and updating the screen
;;
;; The line numbers need to be repainted whenever the window moves, or
;; when an insertion or deletion changes the number of line in a file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun different-lines (p1 p2)
  "Returns t if the 2 positions given are on different lines"
  (string-match "\n" (buffer-substring p1 p2)))

(defun lineno-get-from ()
  "Computes the initial position for the renumbering
returns nil if no renumbering needed"
  (let ((point (point))
        (from (if (/= (window-start) lineno-window-start) (window-start)
                lineno-update-from)))
    (when from
      (goto-char from)
      (prog1 (line-beginning-position)
        (goto-char point)))))

;;; The post-command-hook function to repaint the line numbers, if needed.
(defun lineno-post-command (&optional from)
  "Repaints the visible line numbers when the view or contents change"
  (when (and (sit-for 0)                ; forces screen update if no commands waiting
             (or from (setq from (lineno-get-from))))
    (let* ((point (point))
           (line (1+ (count-lines 1 from)))
           (same (count-lines (window-start) from))
           (reuse (nthcdr same lineno-overlays))
           (tag-width (lineno-tag-width from line)))
      (and lineno-overlays (setf (nthcdr same lineno-overlays) nil))
      (setq lineno-overlays (append lineno-overlays
                                    (lineno-update-linenos from line reuse tag-width))
            lineno-window-start (window-start)
            lineno-update-from nil)
      (goto-char point)
      (move-overlay lineno-overlay (window-start) (window-end)))))


;;; An attempt to get it working for shell mode: doesn't work properly
                                        ;(defadvice comint-send-input (after lineno-shell-function disable)
                                        ;  (debug)
                                        ;  (lineno-post-command))


;;; Records buffer changes so the necessary line numbers can be updated.
;;; Called when changes are made to the visible portion of the buffer
(defun lineno-modification (overlay after begin end &optional length)
  (and (different-lines begin end)
       (<= begin (or lineno-update-from (buffer-size)))
       (setq lineno-update-from begin)))

;;; Internal function, updates the visible line numbers. Does not preserve point.
(defun lineno-update-linenos (from line reuse width)
  "Inserts line number overlays starting from position FROM and line number LINE.
REUSE is a list of overlays which can be reused rather than always getting new
lisp objects."
  (let* ((to (window-end))
         (overlays ())
         overlay)
    (goto-char from)
    (beginning-of-line)
    (while (< (point) to)
      (if (setq overlay (pop reuse))
          (move-overlay overlay (point) (point))
        (setq overlay (make-overlay (point) (point)))
        (overlay-put overlay 'lineno t))
      (overlay-put overlay 'before-string (lineno-tag line width))
      (setq overlays (cons overlay overlays)
            line (1+ line))
      (forward-line 1)
      )
    (mapcar 'delete-overlay reuse)
    (reverse overlays)))

;;; Manufactures the text which is used for line numbers
(defun lineno-tag (num width)
  (setq width (1+ width))
  (let ((text (format "      %s " num)))
    (setq text (substring text (- (length text) width)))
    (put-text-property 0 width 'face lineno-face text)
    text))

;;; computes the number of chars to put in the tags
(defun lineno-tag-width (from line)
  (ceiling (log10 (+ 1 line (count-lines from (window-end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Command functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lineno-minor-mode (&optional force)
  "Switches on and off lineno-minor-mode.
With FORCE >= 0 it turns it on, < 0 turns it off, otherwise it toggles
the mode.

lineno-minor-mode displays line numbers on the left side of the screen.
Other line number modes number the entire file, but for large files
they run too slowly to be useful. lineno-minor-mode only numbers the
lines currently visible in the selected window, and only updates them
when needed. This makes it efficient enough to work for any size file.
"
  (interactive)
  (if (or (and (not force) lineno-overlay) (and force (< force 0))) (lineno-off)
    (setq lineno-overlay (make-overlay (window-start) (window-end) nil nil t)
          lineno-window-start -1
          lineno-update-from nil)
    (overlay-put lineno-overlay 'modification-hooks '(lineno-modification))
    (overlay-put lineno-overlay 'insert-in-front-hooks '(lineno-modification))
    (overlay-put lineno-overlay 'insert-behind-hooks '(lineno-modification))
    (overlay-put lineno-overlay 'lineno t)
    (add-hook 'post-command-hook 'lineno-post-command nil t)))


(defun lineno-off ()
  "Turns off lineno-minor-mode"
  (interactive)
  (remove-hook 'post-command-hook 'lineno-post-command)
  (save-restriction
    (widen)
    (mapcar 'delete-overlay lineno-overlays)
    (if lineno-overlay (delete-overlay lineno-overlay))
    (setq lineno-overlays nil
          lineno-overlay nil
          lineno-lineno-width nil
          lineno-window-start nil)))

(defun lineno-cleanup ()
  "Turns off lineno-minor-mode, including cleanup if there is a problem
Mainly used during debugging phase, this cleans up extraneous overlays which
could be left up after a problem."
  (interactive)
  (lineno-off)
  (save-restriction
    (widen)
    (mapcar (lambda (overlay) (if (overlay-get overlay 'lineno)
                                  (delete-overlay overlay)))
            (overlays-in 0 (point-max)))))

(provide 'lineno)

