;;; kill-ring-search.el --- incremental search for the kill ring
;;
;; Copyright (C) 2006,2007 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.1
;; Keywords: convenience, matching
;; URL: http://nschum.de/src/emacs/kill-ring-search/kill-ring-search.el
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;; To install, add the following to your .emacs file:
;; (autoload 'kill-ring-search "kill-ring-search"
;;  "Search the kill ring in the minibuffer."
;;  (interactive))
;; (global-set-key "\M-\C-y" 'kill-ring-search)
;;
;; Just call kill-ring-search and enter your search.
;; M-y and C-y work as usual.  You can also use C-r like in a shell.
;; C-v, M-v, C-n and C-p will scroll the view.
;;
;;; Change Log:
;;
;; 2007-05-15 (1.1)
;;    Added compatibility to icomplete-mode.
;;    Added scrolling support.
;;
;; 2006-10-12 (1.0)
;;    Initial release.
;;
;;; Code:

(require 'cl)

(defvar kill-ring-search-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map "\C-r" 'kill-ring-search-prev)
    (define-key map "\M-y" 'kill-ring-search-prev)
    (define-key map "\C-y" 'exit-minibuffer)
    (define-key map "\C-v" 'kill-ring-scroll-up-page)
    (define-key map "\M-v" 'kill-ring-scroll-down-page)
    (define-key map "\C-n" 'kill-ring-scroll-up)
    (define-key map "\C-p" 'kill-ring-scroll-down)
    map)
  "*Keymap used inside the minibuffer by `kill-ring-search'.")

(defvar kill-ring-case-fold nil
  "*Non-nil if `kill-ring-search' should ignore case.")

(defvar kill-ring-search-pos nil
  "The remaining parts of the kill-ring to be searched by `search-kill-ring'.")

(defvar kill-ring-search-string nil
  "The current string searched for by `search-kill-ring'.")

(defvar kill-ring-search-calling-buffer nil
  "The buffer from which the current `search-kill-ring' originated.")

(defvar kill-ring-search-eoinput nil
  "Point where minibuffer input ends and completion info begins.")
(make-variable-buffer-local 'kill-ring-search-eoinput)

(defvar kill-ring-scroll-pos nil)

(defvar kill-ring-auto-scroll-pos nil)

(defun kill-ring-search-pre-command ()
  "Remove the current `kill-ring-search' match before minibuffer input."
  (delete-region kill-ring-search-eoinput (point-max)))

(defsubst kill-ring-search-substring (search-string string)
  "Search SEARCH-STRING in STRING while honoring `kill-ring-case-fold'."
  (if case-fold-search
      (search (downcase search-string) (downcase string))
    (search search-string string)))

(defun get-next-match (search-list)
  "Search SEARCH-LIST for a match on `kill-ring-search-string'."
  (let ((ring search-list)
        (res nil))
    (while (and ring (null res))
      (if (kill-ring-search-substring kill-ring-search-string (car ring))
          (setq res (car ring))
        (setq ring (cdr ring))))
    ring))

(defun kill-ring-chop-newline (text)
  "Chop off trailing newline in TEXT if any."
  (let ((last (1- (length text))))
    (if (and (>= last 0)
             (eq ?\n (elt text last)))
        (substring text 0 last)
      text)))

(defun kill-ring-search-create-highlighted-match
  (string search-string max-lines first-line)
  "Return a copy of STRING that highlights the the `kill-ring-search'.
If FIRST-LINE is set, start with that line, otherwise start with a line so
that SEARCH-STRING is visible."
  (if string
      (let ((lines (split-string string "^")))
        (when (equal (car lines) "")
          (pop lines))
        (dotimes (i (or first-line 0))
          (pop lines))
        (let* ((display-start (cons nil nil))
               (display-tail display-start)
               (no-match-yet (null first-line)))
          (while (and lines (or (>= (decf max-lines) 0)
                                no-match-yet))
            (let* ((line (pop lines))
                   (pos (kill-ring-search-substring search-string line)))
              (when pos
                (add-text-properties pos (+ pos (length search-string))
                                     '(face highlight) line)
                (setq no-match-yet nil))
              (setcdr display-tail (setq display-tail (cons line nil)))
              (when (<= max-lines 0)
                ;; pop off the beginning, so we don't produce too many lines
                (pop display-start))))
          (setq kill-ring-auto-scroll-pos
                (- (or first-line 0)
                   (if (< max-lines -1) (+ 1 max-lines) 0)))
          (kill-ring-chop-newline (apply 'concat display-start))))
    "NO MATCH"))

(defun kill-ring-search-post-command ()
  "Display the current `kill-ring-search' match after minibuffer input occured."
  (let ((contents (buffer-substring (minibuffer-prompt-end) (point-max))))
    (setq kill-ring-search-eoinput (point-max))
    (save-excursion
      (goto-char (point-max))
      (setq kill-ring-search-string contents)
      (let ((match (get-next-match kill-ring-search-pos)))
        (unless match
          ;; reset, if nothing was found
          (setq kill-ring-scroll-pos nil)
          (setq kill-ring-search-pos kill-ring)
          (setq match (get-next-match kill-ring-search-pos)))
        (insert "\n" (kill-ring-search-create-highlighted-match
                      (car match)
                      kill-ring-search-string
                      (- (floor (kill-ring-search-max-minibuffer-size)) 2)
                      kill-ring-scroll-pos))))))

(defun kill-ring-search-max-minibuffer-size ()
  "Return the maximum size the minibuffer can get."
  (if resize-mini-windows
      (cond ((floatp max-mini-window-height)
             (* (frame-height)
                max-mini-window-height))
            ((integerp max-mini-window-height)
             max-mini-window-height)
            (t 1))
    1))

(defun kill-ring-search-minibuffer-setup ()
  "Set up the minibuffer for `kill-ring-search' completions."
  (add-hook 'post-command-hook 'kill-ring-search-post-command nil t)
  (add-hook 'pre-command-hook 'kill-ring-search-pre-command nil t)
  (with-current-buffer kill-ring-search-calling-buffer
    (remove-hook 'minibuffer-setup-hook 'kill-ring-search-minibuffer-setup))
  (setq kill-ring-search-calling-buffer nil))

;;;###autoload
(defun kill-ring-search ()
  "Search the kill ring in the minibuffer."
  (interactive)
  (let ((minibuffer-local-completion-map kill-ring-search-keymap)
        (iswitchb-require-match t))
    (setq kill-ring-search-eoinput (point-max))
    (setq kill-ring-scroll-pos nil)
    (setq kill-ring-search-calling-buffer (current-buffer))
    (setq kill-ring-search-pos kill-ring)
    (setq kill-ring-search-string "")
    (let ((minibuffer-setup-hook))
      (add-hook 'minibuffer-setup-hook 'kill-ring-search-minibuffer-setup)
      (completing-read "Kill ring search: " '(("dummy" . 1)) nil nil nil nil)
      (let ((result (car-safe (get-next-match kill-ring-search-pos))))
        (unless result (error "No match"))
        (insert result))
      (setq kill-ring-search-pos kill-ring)
      (setq kill-ring-search-string ""))))

(defun kill-ring-scroll-up (&optional arg)
  (interactive "p")
  (setq kill-ring-scroll-pos
        (max 0 (+ arg (or kill-ring-scroll-pos kill-ring-auto-scroll-pos 0)))))

(defun kill-ring-scroll-down (&optional arg)
  (interactive "p")
  (kill-ring-scroll-up (- arg)))

(defun kill-ring-scroll-up-page (&optional arg)
  (interactive "p")
  (kill-ring-scroll-up (- (window-height) 1 next-screen-context-lines)))

(defun kill-ring-scroll-down-page (&optional arg)
  (interactive "p")
  (kill-ring-scroll-down (- (window-height) 1 next-screen-context-lines)))

(defun kill-ring-search-prev ()
  "Return the previous match also matching the current `kill-ring-search'"
  (interactive)
  (let ((new (get-next-match (cdr (get-next-match kill-ring-search-pos)))))
    (if new
        (progn
          (setq kill-ring-search-pos new)
          (setq kill-ring-scroll-pos 0))
      (beep))))

(provide 'kill-ring-search)

;;; kill-ring-search.el ends here
