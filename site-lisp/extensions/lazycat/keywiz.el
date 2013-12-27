;;; keywiz.el --- Emacs key sequence quiz

;; Copyright (C) 2002, 2003 Jesper Harder

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Created: 15 Apr 2002
;; Version: 1.4
;; Location: <http://purl.org/harder/>
;; Keywords: games, keyboard

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:

;; keywiz.el drills you about Emacs key-bindings.  You're presented
;; with the name of a command and the docstring, and then prompted for
;; the correct key sequence.  You'll earn one point for each correct
;; answer during the time limit.
;;
;; Invoke with `M-x keywiz'.  A prefix argument will force keywiz to
;; rescan the key-binding -- this is useful if you want to include
;; bindings from a different mode.
;;
;; Are you a true Emacs key-binding wizard or just a poor vi looser?
;; Get your foot-pedals in position and see how many key-bindings you
;; can remember in two minutes.
;;
;; Forget about your Nethack high-score -- surely, knowing how to
;; wield the powers of the One True Editor at your fingertips will
;; earn you more bragging rights than ascending bare-footed in some
;; silly game with vi-keybindings :-)

;;; History:

;; Changes in version 1.4:
;;
;; * Copy the current local and global keymap to the keywiz buffer.
;; * Exclude `undefined'.
;;
;; Changes in version 1.3:
;;
;; Don't require cl at run-time.  Doc and customize fixes.  Exclude
;; `select-window'.
;;
;; Changes in version 1.2:
;; Patch from  Luke Gorrie <luke@bluetail.com>:
;;
;; * Cache key-bindings to make start up faster.
;;
;; * Pressing `r' will pause keywiz and enter a recursive edit in the
;;   *scratch* buffer.
;;
;; Changes in version 1.1:
;;    It now works in XEmacs -- patch from Hrvoje Niksic
;;    <hniksic@arsdigita.com>

;;; Code:

(require 'timer)

(eval-when-compile
  (require 'cl))

(require 'gamegrid)

(defgroup keywiz nil
  "Emacs key sequence quiz."
  :version "21.2"
  :group 'games
  :group 'keyboard
  :link '(emacs-commentary-link "keywiz.el"))

(defface keywiz-wrong-face
  '((t (:foreground "Red")))
  "Face for wrong answers."
  :group 'keywiz)

(defface keywiz-right-face
  '((t (:foreground "dark green")))
  "Face for right answers."
  :group 'keywiz)

(defface keywiz-command-face
  '((t (:foreground "Blue"
                    :weight bold
                    :height 1.2
                    :inherit 'variable-pitch)))
  "Face for the Emacs commands."
  :group 'keywiz)

(defface keywiz-heading-face
  '((t (:weight bold
                :height 1.5
                :inherit 'variable-pitch)))
  "Face for headings."
  :group 'keywiz)

(defcustom keywiz-brief-flag t
  "Non-nil means that only the first line of the doc-string is displayed."
  :type '(choice (const t)
                 (const nil))
  :group 'keywiz)

(defvar keywiz-cached-commands nil
  "Command list from the previous run.")

(defconst keywiz-right-phrases '("Excellent!" "Yes." "Indeed." "You're right."
                                 "t." "Spot-on." "Correct." "Yep."))
(defconst keywiz-wrong-phrases '("Nope." "Wrong." "Nah." "nil." "No."
                                 "Incorrect." "(beep)" "Huh?" "Nay."))

(defvar keywiz-temp-dir (if (fboundp 'temp-directory)
                            (temp-directory)
                          temporary-file-directory))

(defvar keywiz-score-file
  (if (fboundp 'gamegrid-add-score-insecure)
      "keywiz-scores"
    (expand-file-name "keywiz-scores"
                      keywiz-temp-dir))
  "File for holding high scores.")

(defconst keywiz-time-limit 120)

(defvar keywiz-not-key-regexp
  (regexp-opt
   '("mouse" "frame" "menu-bar" "mode-line" "compose-last-chars"
     "vertical-line" "vertical-scroll-bar" "header-line"
     "select-window")))

(defvar keywiz-global-map nil)
(defvar keywiz-local-map nil)

(when (featurep 'xemacs)
  (defun keywiz-events-to-keys (vector)
    (map 'vector #'identity
         (delq nil (mapcar
                    (lambda (el)
                      (cond ((key-press-event-p el)
                             (let ((mods (event-modifiers el))
                                   (key  (event-key el)))
                               (when (characterp key)
                                 (setq key (intern (make-string 1 key))))
                               (if mods
                                   (append mods (list key))
                                 key)))
                            ((or (symbolp el)
                                 (characterp el)
                                 (listp el))
                             el)))
                    vector)))))

(defun keywiz-key-press-event-p (x)
  "Return t if X is a keyboard event."
  (let ((event (append x nil)))
    (when (consp event)
      (setq event (car event)))
    (if (symbolp event)
        (not (string-match keywiz-not-key-regexp
                           (prin1-to-string
                            (car (get event 'event-symbol-elements)))))
      t)))

(defun keywiz-insert-with-face (face &rest str)
  "Insert STR with face FACE."
  (put-text-property (point)
                     (progn
                       (mapc 'insert str)
                       (point))
                     'face face))

;; Emacs 20.7 doesn't have float-time

(defalias 'keywiz-float-time
  (if (fboundp 'float-time)
      'float-time
    (lambda ()
      (let ((s (current-time)))
        (+ (* (car s) 65536.0) (cadr s))))))

(defun keywiz-random (x)
  "Return random element from list X."
  (nth (random (length x)) x))

(defun keywiz (rescan)
  "Start the key sequence quiz.
If RESCAN (the prefix) is non-nil, force a rescan of the key bindings.
Keywiz uses the key bindings for the buffer where it is invoked.
Press `q' to quit before the two minute time limit is over.  Press `r'
to pause keywiz and enter a recursive edit in the *scratch* buffer."
  (interactive "P")
  (let ((score 0)
        (first t)
        commands start-time timer doc key input quit)
    (random t)
    (message "Finding key bindings...")
    ;; It's probably *much* faster to find the bindings by searching
    ;; the keymaps -- but this is easier:
    (if (and (not rescan) keywiz-cached-commands)
        (progn (setq commands keywiz-cached-commands)
               (message "Finding all key bindings...cached (use C-u to rescan)"))
      (do-all-symbols (sym)
        (when (and (commandp sym)
                   (not (memq sym '(self-insert-command
                                    digit-argument undefined))))
          (let ((keys (apply 'nconc (mapcar
                                     (lambda (key)
                                       (when (keywiz-key-press-event-p key)
                                         (list key)))
                                     (where-is-internal sym)))))
            ;;  Politically incorrect, but clearer version of the above:
            ;;	  (let ((keys (delete-if-not 'keywiz-key-press-event-p
            ;;				     (where-is-internal sym))))
            (and keys
                 (push (list sym keys) commands)))))
      (setq keywiz-cached-commands commands)
      (message "Finding key bindings...done")
      (setq keywiz-global-map (current-global-map)
            keywiz-local-map (current-local-map)))
    (setq start-time (keywiz-float-time))
    
    (switch-to-buffer (get-buffer-create "*keywiz*"))
    (use-global-map keywiz-global-map)
    (use-local-map keywiz-local-map)
    (make-local-variable 'mode-line-format)
    ;; Use unwind-protect to make sure the timer is killed.
    (unwind-protect
        (progn
          (setq timer
                (run-with-timer
                 0 1
                 (lambda ()
                   (setq mode-line-format
                         (list "(keywiz)  Score: " (number-to-string score)
                               " Time left: "
                               (number-to-string
                                (round (- keywiz-time-limit
                                          (- (keywiz-float-time)
                                             start-time))))))
                   (force-mode-line-update))))
          (erase-buffer)
          (keywiz-insert-with-face
           'keywiz-heading-face "Welcome to keywiz\n")
          (insert "There are currently " (number-to-string (length commands))
                  (substitute-command-keys
                   " commands.  Your current score and the time left is
displayed in the mode line.  Press `q' to quit before the time limit is over.
Press `r' to pause and enter a recursive edit, `\\[exit-recursive-edit]' (exit-recursive-edit)
will return to keywiz.  This allows you to try out a command."))
          (while (and commands
                      (not quit)
                      (< (- (keywiz-float-time) start-time)
                         keywiz-time-limit))
            (setq commands (remove (setq key (keywiz-random commands))
                                   commands))
            (keywiz-insert-with-face 'keywiz-command-face
                                     "\n\n"  (prin1-to-string (car key)))
            (when (setq doc (documentation (car key)))
              (insert "\n" (if keywiz-brief-flag
                               (substring doc 0 (string-match "\n" doc))
                             doc)))
            (insert "\n")
            ;; We need to recenter the first one differently because the
            ;; intro is longer.
            (if (not first)
                (recenter 6)
              (recenter 8)
              (setq first nil))
            (if (featurep 'xemacs)
                (setq input (keywiz-events-to-keys (read-key-sequence "?")))
              (setq input (read-key-sequence-vector "?")))
            (cond
             ((member input (cadr key))
              (keywiz-insert-with-face
               'keywiz-right-face
               (keywiz-random keywiz-right-phrases)
               "  The answer is: "
               (mapconcat 'key-description (cadr key) ", "))
              (incf score))
             ((setq quit (and (member input '([?q] [q]))
                              (y-or-n-p "Do you want to quit? "))))
             ((member input '([?r] [r]))
              ;; 'r' means recursive muck-around with the timer stopped
              (keywiz-insert-with-face
               'keywiz-wrong-face "Time-out time-out! Question skipped.")
              (cancel-timer timer)
              (let ((timeout-start (keywiz-float-time)))
                (cancel-timer timer)
                (save-excursion
                  (save-window-excursion
                    (switch-to-buffer-other-window
                     (get-buffer-create "*scratch*"))
                    (recursive-edit)))
                (timer-activate timer)
                (incf start-time (- (keywiz-float-time) timeout-start))))
             (t
              (keywiz-insert-with-face
               'keywiz-wrong-face
               (keywiz-random keywiz-wrong-phrases)
               "  The correct answer is: "
               (mapconcat 'key-description (cadr key) ", "))))))
	  (cancel-timer timer))
    (keywiz-insert-with-face 'keywiz-heading-face "\n\nTime's up\n")
    (insert "You made " (number-to-string score) " points.")
    (gamegrid-add-score keywiz-score-file score)))

(provide 'keywiz)

;;; keywiz.el ends here
