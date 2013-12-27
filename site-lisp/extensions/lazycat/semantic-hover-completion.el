;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Maciej Katafiasz <[EMAIL PROTECTED]>
;; Keywords: semantic completion programming
;; Version: 0.1

;; This file is NOT a part of GNU Emacs.
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode provides hover electric completion based on Semantic Bovinator.
;; "Hover electric" means roughly the same as what is common as IntelliSense
;; in various IDEs, ie. it pops up automatically, and doesn't interfere with
;; typing.
;; You can navigate candidates using the same keys as `next-line' and
;; `previous-line'. To commit, use RET, C-j or C-TAB. See
;; variable `semantic-hover-completion-keys' for details.
;;
;; Installation:
;; - A working installation of CEDET beta is required, particularly Semantic
;;   Bovinator is used.
;; - Place "semantic-hover-completion.el" somewhere in your load-path, then
;;   call "(require 'semantic-hover-completion)" in your ~/.emacs.
;; - You will now need to define appropriate electric strings to get the
;;   automatic pop-up functionality, the function
;;   `semantic-hover-completion-install-c-hooks' provides default hooks for C.
;; - Binding `semantic-hover-completion-popup' to a discrete key is also an
;;   idea, I myself use M-?, which happens to be the same as S-M-/ on US QWERTY
;;   layout, and that's very close to `dabbrev-expand' keybinding

;; Release history:
;; - 0.1: first public release

;;; Code:

(require 'semantic-ia)

(defun list-until (list pred)
  "Remove initial elements in LIST until PRED (which is called on each element)
returns t. It doesn't copy the list, just returns the first matching car."
  (if (and (listp list)
           (not (null list))
           (not (funcall pred (car list))))
      (list-until (cdr list) pred)
    list))

(defun propertize-nth (elems list &rest props)
  "Return a copy of list with elements (which must be suitable for passing to
`propertize') at indexes in elems subjected to `propertize'.
Elems might be an integer, or a list of integers."
  (if (listp list)
      (if props
          (let ((elems (if (listp elems) elems (list elems)))
                (idx 0))
            (mapcar (lambda (elt)
                      (incf idx)
                      (if (member (- idx 1) elems)
                          (apply 'propertize elt props)
                        elt))
                    list))
        list)
    (error "Not a list: %s" list)))

(defun propertize-all (list &rest props)
  "Return a copy of list with all elements (which must be suitable for passing
to `propertize') subjected to `propertize'."
  (if (listp list)
      (if props
          (let ((idx 0))
            (mapcar (lambda (elt)
                      (incf idx)
                      (apply 'propertize elt props))
                    list))
        list)
    (error "Not a list: %s" list)))

(defface semantic-hover-completion-highlight-face
  '((t :inherit semantic-hover-completion-face :weight bold))
  "Face for highlighted tooltip item."
  :group 'semantic-hover-completion)

(defface semantic-hover-completion-face
  '((t :inherit tooltip))
  "Face for tooltips."
  :group 'semantic-hover-completion)

(defcustom semantic-hover-completion-keys
  '((navigate-candidates-forward  [down] [(control n)])
    (navigate-candidates-backward [up]   [(control p)])
    (commit-candidate             [return]  [(control j)] [tab])
    (try-complete-candidate       [(meta tab)])
    (unfilter-candidates          [backspace])
    (dismiss-popup                [esc]  [(control g)]))
  "Keys to be used with completion tooltip. Any keys not specified here
will trigger `semantic-hover-completion-dismiss-or-filter'."
  :group 'semantic-hover-completion
  :type 'alist
  :options '(navigate-candidates-forward
             navigate-candidates-backward))

(defcustom semantic-hover-completion-y-fuzz -9
  "Number of pixels to shift the completion tooltip vertically by. Basically
Emacs tooltip displaying sucks wrt. specifying coordinates, thus the need
for this setting. If the tooltip overlaps your line, try tweaking this value.

If you happen to know a reliable way to determine the vertical offset induced
by menu-bar, tell me, I will be able to remove this silliness then."
  :group 'semantic-hover-completion
  :type 'integer)

(defvar semantic-hover-completion-active nil
  "Non-nil if hover completion is currently active, ie. the tooltip is shown,
then the integer value signifies which completion is currently highlighted.")
(make-variable-buffer-local 'semantic-hover-completion-active)

(defvar semantic-hover-completion-candidates nil
  "Non-nil if hover completion is currently active, in which case it holds
a list of (POS CANDIDATES ORIG-CANDIDATES CTX). POS is an integer indicating
how many
self-inserting keys were entered since the ORIG-CANDIDATES were created. If it's
0, CADIDATES is equal to ORIG-CANDIDATES, otherwise it's a filtered down list
with
candidates matching the subsequent input. CTX is the semantic context in which
completions were obtained.")
(make-variable-buffer-local 'semantic-hover-completion-candidates)

(defun semantic-hover-completion-prepare-keymap ()
  "Prepare `semantic-hover-completion-keymap' for use. Called by
`semantic-hover-completion-popup' immediately before showing the tooltip, to
ensure
keys go to the right place."
  (set-keymap-parent semantic-hover-completion-keymap (car
                                                       (current-active-maps))))

(defun semantic-hover-completion-make-keymap ()
  "Make keymap used by hover completion mode, based on
`semantic-hover-completion-keys'"
  (let ((keymap (make-sparse-keymap)))
    (mapcar (lambda (elt)
              (define-key keymap
                elt (lambda () (interactive)
                      (semantic-hover-completion-do-popup 1))))
            (cdr (assq 'navigate-candidates-forward
                       semantic-hover-completion-keys)))
    (mapcar (lambda (elt)
              (define-key keymap
                elt (lambda () (interactive)
                      (semantic-hover-completion-do-popup -1))))
            (cdr (assq 'navigate-candidates-backward
                       semantic-hover-completion-keys)))
    (mapcar (lambda (elt)
              (define-key keymap elt 'semantic-hover-completion-dismiss))
            (cdr (assq 'dismiss-popup semantic-hover-completion-keys)))
    (mapcar (lambda (elt)
              (define-key keymap elt 'semantic-hover-completion-commit))
            (cdr (assq 'commit-candidate semantic-hover-completion-keys)))
    (mapcar (lambda (elt)
              (define-key keymap elt 'semantic-hover-completion-unfilter))
            (cdr (assq 'unfilter-candidates semantic-hover-completion-keys)))
    ;; By default we want all unspecified keys to dismiss the pop-up and spill
    ;; over
    ;; to default actions that would be taken otherwise.
    (define-key keymap [t] 'semantic-hover-completion-dismiss-or-filter)
    keymap))

(defvar semantic-hover-completion-keymap (semantic-hover-completion-make-keymap)
  "Keymap to be used with completion tooltip. It needs to be prepared before
actual use
to have the correct parent, use `semantic-hover-completion-prepare-keymap' for
that.")

(defun semantic-hover-completion-guesstimate-y-offset ()
  "Attempt to guess how much we need to shift the tooltip to get it displayed
right
below the current line. See `semantic-hover-completion-y-fuzz' docstring."
  (let ((minibuf (window-pixel-edges (car (cdr (window-tree)))))
        (frame (frame-pixel-height)))
    (+ (* 2 (- frame (nth 3 minibuf)))
       (frame-char-height)
       semantic-hover-completion-y-fuzz)))

(defun semantic-hover-completion-make-candidates ()
  (let ((ctx (semantic-analyze-current-context (point))))
    (cons (semantic-analyze-possible-completions (point))
          ctx)))

(defun semantic-hover-completion-get-candidates-names ()
  (mapcar (lambda (elt) (concat (semantic-format-tag-prototype elt) "\n"))
          (nth 1 semantic-hover-completion-candidates)))

(defun semantic-hover-completion-do-popup (n)
  "Do the actual work of showing and navigating the completion pop-up. Integer
argument n
specifies how many positions should in the pop-up should the highlight be
moved."
  (when semantic-hover-completion-active
    (let* ((strings (semantic-hover-completion-get-candidates-names))
           (pos (posn-x-y (posn-at-point)))
           (win (window-inside-pixel-edges))
           (pos (cons (+ (car pos) (nth 0 win))
                      (+ (cdr pos) (nth 1 win)
                         (semantic-hover-completion-guesstimate-y-offset)))))

      (incf semantic-hover-completion-active n)
      (unless (< semantic-hover-completion-active (length strings))
        (setq semantic-hover-completion-active 0))
      (unless (>= semantic-hover-completion-active 0)
        (setq semantic-hover-completion-active (1- (length strings))))

      (setq strings (propertize-nth semantic-hover-completion-active
                                    (propertize-all strings 'face
                                                    'semantic-hover-completion-face)
                                    'face
                                    'semantic-hover-completion-highlight-face))
      (x-show-tip (apply 'concat strings) nil
                  (list (cons 'left (car pos))
                        (cons 'top (cdr pos)))
                  100))))

(defun semantic-hover-completion-popup ()
  "Prepare keymaps, then call `semantic-hover-completion-do-popup' to show
the completion pop-up."
  (interactive)
  (let ((cands (semantic-hover-completion-make-candidates)))
    (when cands
      (setq semantic-hover-completion-candidates
            ;; `copy-sequence' necessary, otherwise they both point to the
            ;; same place, and that's just not good
            (list 0 (car cands) (copy-sequence (car cands)) (cdr cands)))
      (semantic-hover-completion-prepare-keymap)
      (add-to-list 'minor-mode-map-alist
                   (cons 'semantic-hover-completion-active
                         semantic-hover-completion-keymap))
      (setq semantic-hover-completion-active 0)
      (semantic-hover-completion-do-popup 0))))

(defun semantic-hover-completion-dismiss ()
  "Dismiss the active completion tooltip, undoing any keymap changes done
during pop-up."
  (interactive)
  (remove 'minor-mode-map-alist
          (cons 'semantic-hover-completion-active
                semantic-hover-completion-keymap))
  (setq semantic-hover-completion-active nil))

(defun semantic-hover-completion-dismiss-or-filter ()
  "Possibly dismiss the popup and replay the last input event, if it would be
something
else than `self-insert-command', otherwise insert the character and filter down
candidates.

This function MUST be bound to a default keymap entry (ie, with t as the key),
otherwise
it'll fail to recognise which events need replaying and bad things will happen."
  (interactive)
  (if (not (eq (key-binding (this-command-keys-vector)) 'self-insert-command))
      (progn
        (semantic-hover-completion-dismiss)
        (add-to-list 'unread-command-events last-input-event t))
    (self-insert-command 1)
    (semantic-hover-completion-filter (aref (this-command-keys) 0))
    ;; Update POS if we're still displaying the tooltip
    (when semantic-hover-completion-active
      (incf (nth 0 semantic-hover-completion-candidates)))))

(defun semantic-hover-completion-filter (char)
  "Insert self-inserting keystrokes, filtering down candidates list based on
input."
  (when semantic-hover-completion-active
    (let ((idx semantic-hover-completion-active)
          (cands (nth 1 semantic-hover-completion-candidates))
          (pos (nth 0 semantic-hover-completion-candidates)))
      (setf cands
            (delete nil
                    (mapcar (lambda (elt)
                              (let ((name (semantic-tag-name elt)))
                                (when (< pos (length name))
                                  (and (equal char (aref name pos))
                                       elt))))
                            cands)))
      (setf (nth 1 semantic-hover-completion-candidates) cands)
      (if cands
          (progn
            (semantic-hover-completion-do-popup 0))
        (semantic-hover-completion-dismiss)))))

(defun semantic-hover-completion-unfilter ()
  "Handle deletion of previously entered filter keystrokes, widening the
candidate list.
Possibly try also going before the original starting point, if semantic context
indicates
it's possible, though that's a fairly expensive operation requiring recomputing
the full
candidates list."
  (interactive)
  ;; Shit fucking blows up when I try to do this, more or less, so
  ;; comment it out for now and stick to plain backward-delete-char.
  ;; But as soon as I kick emacs's dirty hackish arse, I'm gonna make
  ;; it fall through to the default binding, so you can do stuff like
  ;; have C-Backspace delete whole word as normal, without dismissing
  ;; the completion and doing any setup for it whatsoever, other than
  ;; putting C-Backspace in known 'unfilter-candidates keys.
  ;;   (let ((semantic-hover-completion-active nil))
  ;;     (add-to-list 'unread-command-events last-input-event t)
  ;;     (read-event))
  (backward-delete-char)
  (when semantic-hover-completion-active
    (let ((cands (nth 1 semantic-hover-completion-candidates))
          (pos (nth 0 semantic-hover-completion-candidates)))
      (if (> pos 0)
          ;; Take prefix from any of the current matches, since by definition
          ;; they have to share POS first chars.
          (let ((pref (substring
                       (semantic-tag-name (car cands))
                       0 (incf (nth 0 semantic-hover-completion-candidates)
                               -1))))
            ;; Replay filters up to the second to last one
            (setf (nth 0 semantic-hover-completion-candidates) 0)
            (setf (nth 1 semantic-hover-completion-candidates)
                  (nth 2 semantic-hover-completion-candidates))
            (mapcar (lambda (char)
                      (semantic-hover-completion-filter char)
                      (when semantic-hover-completion-active
                        (incf (nth 0 semantic-hover-completion-candidates))))
                    pref))
        (let ((ctx (nth 3 semantic-hover-completion-candidates)))
          (cond
           ((> (- (cdr (oref ctx bounds)) (car (oref ctx bounds))) 0)
            (semantic-hover-completion-dismiss)
            (semantic-hover-completion-popup))
           (t (semantic-hover-completion-dismiss))))))))

(defun semantic-hover-completion-commit ()
  "Commit the selected candidate and dismiss the pop-up."
  (interactive)
  (when semantic-hover-completion-active
    (let ((idx semantic-hover-completion-active)
          (ctx (nth 3 semantic-hover-completion-candidates))
          (cands (nth 1 semantic-hover-completion-candidates))
          (pos (nth 0 semantic-hover-completion-candidates)))
      (delete-region (car (oref ctx bounds)) (cdr (oref ctx bounds)))
      (delete-region (- (point) pos) (point))
      (semantic-ia-insert-tag (nth idx cands))
      (semantic-hover-completion-dismiss))))

(defmacro semantic-hover-completion-add-electric-string (str)
  "Cause the string specified by str be electric, ie. whenever the last character
of
it is input in the buffer, and forms the complete string, it will cause
`semantic-hover-completion-popup' to fire. The last character of all electric
strings
needs to be unique, otherwise new electric strings will cancel previously
defined ones.
Strings are electric locally to the major mode, so this macro's output is a
perfect
candidate for running in a mode hook.

Str must contain only ordinary characters, ie. ones for which
`self-insert-command' would
normally be defined. It might be a string literal or a symbol, but its value
must be defined
at the time this macro is called."
  ;; Eval all references to str to allow both literals and symbols with string
  value
  (if (stringp (eval str))
      `(lambda ()
         (local-set-key [,(aref (eval str) (1- (length (eval str))))]
                        (lambda ()
                          (interactive)
                          (let ((succ nil))
                            (self-insert-command 1)
                            (save-excursion
                              (let ((start (- (point) ,(length (eval str))))
                                    (end (point)))
                                (goto-char start)
                                (save-match-data
                                  (search-forward ,str end t)
                                  (if (= (match-end 0) (point))
                                      (setq succ t)))))
                            ;; Actual call is done here to avoid point being
                            returned
                            ;; to the starting position by `save-excursion'
                            (if succ
                                (semantic-hover-completion-popup))))))
    `(error "str must be a string of ordinary characters")))

(defun semantic-hover-completion-install-c-hooks ()
  "Install default hooks to add electric strings suitable for
C and C++ modes."
  (interactive)
  (add-to-list 'c-mode-hook
               (semantic-hover-completion-add-electric-string "->"))
  (add-to-list 'c++-mode-hook
               (semantic-hover-completion-add-electric-string "->"))
  (add-to-list 'c-mode-hook
               (semantic-hover-completion-add-electric-string "."))
  (add-to-list 'c++-mode-hook
               (semantic-hover-completion-add-electric-string "."))
  (add-to-list 'c++-mode-hook
               (semantic-hover-completion-add-electric-string "::")))

(provide 'semantic-hover-completion)
;;; semantic-hover-completion.el ends here
