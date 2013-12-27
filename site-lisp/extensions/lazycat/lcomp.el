;;; lcomp.el --- list-completion hacks!

;; Copyright (C) 2002 by Taiki SUGAWARA <taiki.s@cityfujisawa.ne.jp>

;; Author: Taiki SUGAWARA <taiki.s@cityfujisawa.ne.jp>
;; Keywords: tool

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

;; This package adds keybindings to the completions buffer:
;;
;;    "\C-i"      -> next-completion
;;    "\M-\C-i"   -> previous-completion
;;    "f"         -> next-completion
;;    "b"         -> previous-completion
;;    "n"         -> next-line
;;    "p"         -> previous-line
;;    " "         -> scroll-up
;;    [del]       -> scroll-down
;;    [backspace] -> scroll-down
;;    "q"         -> delete-completion-window
;;
;; and a global keybinding:
;;
;;    "\M-v"      -> lcomp-select-completion-window-or-scroll-down
;;
;; You may also enable advice to other Emacs functions to make the
;; completions buffer window disappear after use.  The completions buffer
;; is usually only dismissed after completion when it is created from
;; minibuffer completion, but this advice makes it get dismissed correctly
;; from any buffer (e.g. shell, or by calling
;; `comint-dynamic-complete-filename').  To enable this advice, put these
;; lines into your ~/.emacs.
;;
;;     (require 'lcomp)
;;     (lcomp-activate-advices t)
;;
;; Or alternatively, customize the variable `lcomp-enable' and save the
;; setting for future sessions.

;;; History:
;; 
;;  2003-11-11 Peter S Galbraith <psgdebian.org>
;;   - Create `lcomp-activate-advices' to activate the advice instead of
;;     doing it by default.  This complies with Elisp Coding conventions
;;     that state that loading a file shouldn't install anything.  Perhaps
;;     the same should be done with all the key definitions?
;;   - Create defcustom `lcomp-enable' as an alternate and user-frienfly
;;     method to enable the advice.

;;; Code:

(defvar lcomp-before-completion-winconf nil)
(defvar lcomp-completion-halfway-p nil)
(defvar lcomp-display-completion-buffer-p nil)
(defvar lcomp-completion-buffer nil)

;;; key bindings.
(define-key completion-list-mode-map "\C-i" 'next-completion)
(define-key completion-list-mode-map "\M-\C-i" 'previous-completion)
(define-key completion-list-mode-map "f" 'next-completion)
(define-key completion-list-mode-map "b" 'previous-completion)
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "p" 'previous-line)
(define-key completion-list-mode-map " " 'scroll-up)
(define-key completion-list-mode-map [del] 'scroll-down)
(define-key completion-list-mode-map [backspace] 'scroll-down)
(define-key completion-list-mode-map "q" 'delete-completion-window)
(define-key global-map "\M-v" 'lcomp-select-completion-window-or-scroll-down)

(defadvice try-completion (around lcomp-ad disable)
  (let ((ret ad-do-it))
    (setq lcomp-completion-halfway-p (stringp ret))
    ret))

(defun lcomp-setup-completion ()
  (when (and (not lcomp-before-completion-winconf)
	     (not (window-minibuffer-p)))
    (setq lcomp-display-completion-buffer-p t)
    (setq lcomp-completion-buffer standard-output)
    (setq lcomp-before-completion-winconf (current-window-configuration))))

(add-hook 'completion-setup-hook 'lcomp-setup-completion)

(defvar err)
(defun lcomp-resume-before-completion-winconf-1 ()
  (condition-case nil
      (set-window-configuration lcomp-before-completion-winconf)
    (error
     (message "%s occured. bat ignore." (error-message-string err))))
  (setq lcomp-before-completion-winconf nil)
  (setq lcomp-completion-buffer nil))

(defun lcomp-resume-before-completion-winconf ()
  (when (and lcomp-before-completion-winconf
	     (not (or (and (eq this-command 'self-insert-command)
			   (string-match "\\(\\sw\\|\\s_\\)"
					 (this-command-keys)))
		      (eq (current-buffer) lcomp-completion-buffer)
		      (window-minibuffer-p)
		      lcomp-display-completion-buffer-p
		      lcomp-completion-halfway-p)))
    (let ((buf (current-buffer)))
      (lcomp-resume-before-completion-winconf-1)
      (unless (eq buf (current-buffer))
	(switch-to-buffer buf))))
  (setq lcomp-display-completion-buffer-p nil)
  (setq lcomp-completion-halfway-p nil))

(add-hook 'post-command-hook 'lcomp-resume-before-completion-winconf)

(defadvice choose-completion (after lcomp-ad disable)
  (when lcomp-before-completion-winconf
    (lcomp-resume-before-completion-winconf-1)))

(defadvice delete-completion-window (around lcomp-ad disable)
  (if lcomp-before-completion-winconf
      (lcomp-resume-before-completion-winconf)
    ad-do-it))

(defun lcomp-select-completion-window ()
  (interactive)
  (select-window (get-buffer-window lcomp-completion-buffer)))

(defun lcomp-select-completion-window-or-scroll-down (&optional arg)
  (interactive "P")
  (if lcomp-before-completion-winconf
      (lcomp-select-completion-window)
    (scroll-down arg)))

;;;###autoload
(defun lcomp-activate-advices (on)
  "Activate lcomp advices if ON is non-nil, disable otherwise."
  (interactive "p")
  (cond
   (on
    (message "Enabling lcomp")
    (ad-enable-advice 'try-completion 'around 'lcomp-ad)
    (ad-enable-advice 'choose-completion 'after 'lcomp-ad)
    (ad-enable-advice 'delete-completion-window 'around 'lcomp-ad))
   (t
    (message "Disabling lcomp")
    (ad-disable-advice 'try-completion 'around 'lcomp-ad)
    (ad-disable-advice 'choose-completion 'after 'lcomp-ad)
    (ad-disable-advice 'delete-completion-window 'around 'lcomp-ad)))
  (ad-activate 'try-completion)
  (ad-activate 'choose-completion)
  (ad-activate 'delete-completion-window))

(defgroup lcomp nil
  "list-completion hacks."
  :group 'completion)

(defcustom lcomp-enable nil
  "*Enable advice in lcomp to make completion buffer disappear after use."
  :type 'boolean
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (if (featurep 'lcomp)
	     (lcomp-activate-advices t)))
  :require 'lcomp
  :group 'lcomp)

(provide 'lcomp)

;;; lcomp.el ends here
