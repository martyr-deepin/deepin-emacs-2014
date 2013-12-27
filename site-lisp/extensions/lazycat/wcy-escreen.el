;;; wcy-escreen.el --- enhanced emacs screen

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: ChunYe Wang <ext-chunye.wang@nokia.com>
;; Keywords: 

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

;; 

;;; Code:

;; configuration
(defgroup wcy-escreen nil
  "Window configuration management"
  :group 'wcy-escreen
  :group 'extensions)

(defcustom wcy-escreen-create-screen-hook nil
  "*Hook to run after `escreen-goto-screen' completes.
An example function that can make use of this hook is
`escreen-enable-number-mode-if-more-than-one-screen'."
  :type 'hook
  :group 'wcy-escreen)

(defcustom wcy-escreen-before-goto-screen-hook nil
  "*Hook to run after `escreen-goto-screen' completes.
An example function that can make use of this hook is
`escreen-enable-number-mode-if-more-than-one-screen'."
  :type 'hook
  :group 'wcy-escreen)

(defcustom wcy-escreen-after-goto-screen-hook nil
  "*Hook to run after `escreen-goto-screen' completes.
An example function that can make use of this hook is
`escreen-enable-number-mode-if-more-than-one-screen'."
  :type 'hook
  :group 'wcy-escreen)

;; Keybindings

(defcustom wcy-escreen-prefix-char "\C-\\"
  "*Character prefixing wcy-escreen commands.
If you wish to change this, you must also do

   (global-set-key wcy-escreen-prefix-char 'wcy-escreen-prefix)

to update the prefix in the global keymap."
  :type 'string
  :group 'wcy-escreen)

(defvar wcy-escreen-map nil
  "*Keymap for wcy-escreen commands.")
(cond
 ((null wcy-escreen-map)
  (setq wcy-escreen-map (make-sparse-keymap))
  (define-key wcy-escreen-map wcy-escreen-prefix-char 'wcy-escreen-goto-last-screen)
  (define-key wcy-escreen-map "0" 'wcy-escreen-goto-screen-0)
  (define-key wcy-escreen-map "1" 'wcy-escreen-goto-screen-1)
  (define-key wcy-escreen-map "2" 'wcy-escreen-goto-screen-2)
  (define-key wcy-escreen-map "3" 'wcy-escreen-goto-screen-3)
  (define-key wcy-escreen-map "4" 'wcy-escreen-goto-screen-4)
  (define-key wcy-escreen-map "5" 'wcy-escreen-goto-screen-5)
  (define-key wcy-escreen-map "6" 'wcy-escreen-goto-screen-6)
  (define-key wcy-escreen-map "7" 'wcy-escreen-goto-screen-7)
  (define-key wcy-escreen-map "8" 'wcy-escreen-goto-screen-8)
  (define-key wcy-escreen-map "9" 'wcy-escreen-goto-screen-9)
  (define-key wcy-escreen-map "?" 'wcy-escreen-help)
  (define-key wcy-escreen-map "l" 'wcy-escreen-menu)
  (define-key wcy-escreen-map "a" 'wcy-escreen-get-active-screen-numbers)
  (define-key wcy-escreen-map "b" 'wcy-escreen-get-current-screen-number)
  (define-key wcy-escreen-map "c" 'wcy-escreen-create-screen)
  (define-key wcy-escreen-map "g" 'wcy-escreen-goto-screen)
  (define-key wcy-escreen-map "k" 'wcy-escreen-kill-screen)
  (define-key wcy-escreen-map "t" 'wcy-escreen-set-screen-name)
  (define-key wcy-escreen-map "n" 'wcy-escreen-goto-next-screen)
  (define-key wcy-escreen-map "p" 'wcy-escreen-goto-prev-screen)))

(defalias 'wcy-escreen-prefix wcy-escreen-map)


;;; the following defvar is the core data structure and can
;;; not touched by users

(defvar wcy-escreen-current-obarray nil
  "current obarray used to hash all screen local variable, nil
means that there is no screen at all. its value is the
elements of wcy-escreen-obarray-list." )

(defvar wcy-escreen-obarray-list nil
  "a list of obarray, each element is an obarray, which is
used to store all screen local variable. nil means there is
no screen at all.")

(defvar wcy-escreen-current-screen-number nil
  "an aux variable to record the current screen number. nil
mean there is no screen at all")
(defvar wcy-escreen-last-screen-number nil
  "nil means there is no last escreen number.")
(defvar wcy-escreen-mode-line-format "| |")
                                 

;; all screen will be killed.
;; 
(defun wcy-escreen-uninstall ()
  (interactive)
  (setq global-mode-string nil)
  (setq wcy-escreen-current-screen-number nil)
  (setq wcy-escreen-last-screen-number nil)
  (setq wcy-escreen-obarray-list nil)
  (setq wcy-escreen-current-obarray nil)
  (delq 'wcy-escreen-mode-line-format global-mode-string))


;; the first screen should be created.
(defun wcy-escreen-install ()
  (interactive)
  (setq wcy-escreen-current-obarray (make-vector 1511 0))
  (setq wcy-escreen-obarray-list 
        (cons wcy-escreen-current-obarray wcy-escreen-obarray-list))
  (setq wcy-escreen-last-screen-number 0)
  (setq wcy-escreen-current-screen-number 0)
  (run-hooks 'wcy-escreen-create-screen-hook)
  (global-set-key wcy-escreen-prefix-char wcy-escreen-map)
  (cond
   ((not global-mode-string)
    (setq global-mode-string '("" wcy-escreen-mode-line-format)))
   ((listp global-mode-string)
    (add-to-list 'global-mode-string 'wcy-escreen-mode-line-format t))))
(defun wcy-escreen-is-installed()
  wcy-escreen-current-obarray)
(defun wcy-escreen-reinstall()
  (interactive)
  (wcy-escreen-uninstall)
  (wcy-escreen-install))

(defun wcy-escreen-create-screen ()
  (interactive)
  (run-hooks 'wcy-escreen-before-goto-screen-hook)
  (setq wcy-escreen-current-obarray (make-vector 1511 0))
  (setq wcy-escreen-obarray-list 
        (append wcy-escreen-obarray-list (cons wcy-escreen-current-obarray nil)))
  ;; last-screen-number unchanged.
  (setq wcy-escreen-last-screen-number wcy-escreen-current-screen-number)
  (setq wcy-escreen-current-screen-number (1- (wcy-escreen-number-of-screen)))
  (run-hooks 'wcy-escreen-create-screen-hook)
  (run-hooks 'wcy-escreen-after-goto-screen-hook))

;; 1. the last screen can not be killed. call uninstall instead.
;; 2. after the screen is killed, goto to next screen, if no next screen, goto screen 0;
;; 3. if there is no last screen. goto the first screen.
(defun wcy-escreen-kill-screen ()
  (interactive)
  (if (not wcy-escreen-current-screen-number)
      (error "screen has not been installed yet"))
  (if (eq (wcy-escreen-number-of-screen) 1)
      (error "can not kill the last screen")
    (run-hooks 'wcy-escreen-kill-screen-hook)
    (run-hooks 'wcy-escreen-before-goto-screen-hook)
    (delq wcy-escreen-current-obarray wcy-escreen-obarray-list)
    (setq wcy-escreen-last-screen-number
          (cond
           ((= wcy-escreen-last-screen-number wcy-escreen-current-screen-number)
            (mod (1+ wcy-escreen-last-screen-number) (wcy-escreen-number-of-screen)))
           ((> wcy-escreen-last-screen-number wcy-escreen-current-screen-number)
            (1- wcy-escreen-last-screen-number))
           (t wcy-escreen-last-screen-number)))
    (setq wcy-escreen-current-screen-number 
          (mod (1+ wcy-escreen-current-screen-number) (wcy-escreen-number-of-screen)))
    (setq wcy-escreen-current-obarray (nth wcy-escreen-current-screen-number wcy-escreen-obarray-list))
    (run-hooks 'wcy-escreen-after-goto-screen-hook)))

(defun wcy-escreen-goto-screen (n)
  (interactive "NGo to escreen number: ")
  (let ((oba (nth n wcy-escreen-obarray-list)))
    (if (not oba)
        (error (format "no such screen %d." n))
      (run-hooks 'wcy-escreen-before-goto-screen-hook)
      (setq wcy-escreen-current-obarray oba)
      (setq wcy-escreen-last-screen-number wcy-escreen-current-screen-number)
      (setq wcy-escreen-current-screen-number n)
      (run-hooks 'wcy-escreen-after-goto-screen-hook))))
(defun wcy-escreen-goto-last-screen ()
  (interactive)
  (wcy-escreen-goto-screen wcy-escreen-last-screen-number))
(defun wcy-escreen-goto-screen-0 () (interactive) (wcy-escreen-goto-screen 0))
(defun wcy-escreen-goto-screen-1 () (interactive) (wcy-escreen-goto-screen 1))
(defun wcy-escreen-goto-screen-2 () (interactive) (wcy-escreen-goto-screen 2))
(defun wcy-escreen-goto-screen-3 () (interactive) (wcy-escreen-goto-screen 3))
(defun wcy-escreen-goto-screen-4 () (interactive) (wcy-escreen-goto-screen 4))
(defun wcy-escreen-goto-screen-5 () (interactive) (wcy-escreen-goto-screen 5))
(defun wcy-escreen-goto-screen-6 () (interactive) (wcy-escreen-goto-screen 6))
(defun wcy-escreen-goto-screen-7 () (interactive) (wcy-escreen-goto-screen 7))
(defun wcy-escreen-goto-screen-8 () (interactive) (wcy-escreen-goto-screen 8))
(defun wcy-escreen-goto-screen-9 () (interactive) (wcy-escreen-goto-screen 9))


(defun wcy-escreen-goto-prev-screen ()
  (interactive)
  (if (not wcy-escreen-current-screen-number)
      (error "no screen at all")
    (wcy-escreen-goto-screen (mod (1- wcy-escreen-current-screen-number) (wcy-escreen-number-of-screen)))))
(defun wcy-escreen-goto-next-screen ()
  (interactive)
  (if (not wcy-escreen-current-screen-number)
      (error "no screen at all")
    (wcy-escreen-goto-screen (mod (1+ wcy-escreen-current-screen-number) (wcy-escreen-number-of-screen)))))

(defun wcy-escreen-number-of-screen ()
  (length wcy-escreen-obarray-list))
(defun wcy-escreen-get-local-variable (name &optional default-value)
  (if (not wcy-escreen-current-obarray)
      (error "escreen not installed yet")
    (if (symbolp name) (setq name (symbol-name name)))
    (let ((sym (intern name wcy-escreen-current-obarray)))
      (if (not (boundp sym))
          (set sym default-value))
      (symbol-value sym))))

(defun wcy-escreen-set-local-variable (name value)
  (if (not wcy-escreen-current-obarray)
      (error "escreen not installed yet")
    (if (symbolp name) (setq name (symbol-name name)))
    (let ((sym (intern name wcy-escreen-current-obarray)))
      (set sym value))))
    

;;; related with screen menu
(defun wcy-escreen-map-fun-screen (func)
  (let ((obs wcy-escreen-obarray-list)
        (screen-counter 0))
    (while obs
      (let ((wcy-escreen-current-obarray (car obs)))
        (funcall func)
        (setq obs (cdr obs)
              screen-counter (1+ screen-counter))))))

(defun wcy-escreen-menu-print-a-screen()
  (if (= screen-counter wcy-escreen-current-screen-number)
      (end-of-buffer))
  (save-excursion
    (end-of-buffer)
    (insert 
     (format "%s%d:%-12s -- %-12s\n"
             (if (= screen-counter wcy-escreen-current-screen-number)
                 "* "
               "  ")
             screen-counter
             (wcy-escreen-get-local-variable "screen-name" "*no*")
             (nth 1 (wcy-escreen-get-local-variable "current-buffer" nil))))))
  
(defun wcy-escreen-menu ()
  (interactive)
  (let ((buffer (get-buffer-create "*escreen buffer*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert " Screen Buffers\n --------------- ------------\n")
      (wcy-escreen-map-fun-screen 'wcy-escreen-menu-print-a-screen)
      (wcy-escreen-menu-mode))
    (pop-to-buffer buffer)))

(defvar wcy-escreen-menu-mode-map nil 
  "Keymap for wcy-escreen-menu mode.")
(if wcy-escreen-menu-mode-map
    ()                                  ; Do not change the keymap if it is already set up.
  (setq wcy-escreen-menu-mode-map (make-sparse-keymap))
  (define-key wcy-escreen-menu-mode-map (kbd "<RET>") 'wcy-escreen-menu-mode-goto-screen)
  (define-key wcy-escreen-menu-mode-map (kbd "<SPC>") 'wcy-escreen-menu-mode-next)
  (define-key wcy-escreen-menu-mode-map (kbd "<DEL>") 'wcy-escreen-menu-mode-previous))

(defun wcy-escreen-menu-mode ()
  (fundamental-mode)
  (kill-all-local-variables)
  (use-local-map wcy-escreen-menu-mode-map)
  (setq local-abbrev-table nil)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'escreen-menu-mode)
  (setq mode-name "Escreen Menu")
  (run-hooks 'escreen-menu-mode-hook))

(defvar wcy-escreen-menu-mode-regexp "^[* ] \\([0-9]\\)+:"
  "regexp to recognize the escreen number")
(defun wcy-escreen-menu-mode-search-for-next-screen()
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (if (looking-at wcy-escreen-menu-mode-regexp)
          (list (point) (string-to-int (match-string 1)))))))

(defun wcy-escreen-menu-mode-goto-screen()
  (interactive)
  (let* ((b (current-buffer))
         (w (get-buffer-window b ))
         (num (nth 1 (wcy-escreen-menu-mode-search-for-next-screen))))
    (if num (progn
              (kill-buffer b)
              (if (not (one-window-p)) (delete-window w))
              (wcy-escreen-goto-screen num)))))
  
(defun wcy-escreen-menu-mode-next ()
  (interactive)
  (let (v message-log-max)
    (save-excursion 
      (forward-line)
      (setq v (nth 0 (wcy-escreen-menu-mode-search-for-next-screen))))
    (if v 
        (goto-char v)
      (princ "bottom of screens"))))

(defun wcy-escreen-menu-mode-previous ()
  (interactive)
  (let (v message-log-max)
    (save-excursion 
      (forward-line -1)
      (setq v (nth 0 (wcy-escreen-menu-mode-search-for-next-screen))))
    (if v
        (goto-char v)
      (princ "top of screens"))))


;;;; the following is related with persitance accoss sessions
(require 'session)
(add-hook 'session-before-save-hook 'wcy-escreen-save-session)
(defun wcy-escreen-when-query-kill-emacs ()
  (when (wcy-escreen-is-installed)
    (run-hooks 'wcy-escreen-before-goto-screen-hook))
  t)
(add-hook 'kill-emacs-query-functions 'wcy-escreen-when-query-kill-emacs)
(defun wcy-escreen-convert-to-savable-variable(value)
  (cond
   ((bufferp value) (format "%S " (buffer-name value)))
   ((stringp value)
    (format "%S " value))
   ((consp value)
    (format "(list %s)" 
            (apply 'concat (mapcar 
                            'wcy-escreen-convert-to-savable-variable value))))
   ((numberp value) (format "%d "  value))
   ((eq value t) "t ")
   (t "nil ")))
(defun wcy-escreen-save-session-variables (obs)
  (let ((cmds ""))
    (mapatoms (function (lambda (sym) 
                          ;; (set (intern "symbol-name") symbol-value
                          (setq cmds 
                                (concat cmds 
                                        (format "(set (intern %S wcy-escreen-current-obarray) %s)\n"
                                                (symbol-name sym)
                                                (wcy-escreen-convert-to-savable-variable (symbol-value sym)))))))
              obs)
    cmds))
(defun wcy-escreen-save-session ()
  (when (wcy-escreen-is-installed)
    (insert "(wcy-escreen-install)\n")
    (insert (format 
             "(setq wcy-escreen-obarray-list nil)\n"))
    (loop for screen-counter downfrom (1- (wcy-escreen-number-of-screen)) downto 0 do
          (insert 
           (format ;;; create a new screen
            "(setq wcy-escreen-obarray-list
           (cons (make-vector 1151 0) wcy-escreen-obarray-list))\n"))
          ;; set the current screen 
          (insert "(setq wcy-escreen-current-obarray (car wcy-escreen-obarray-list))\n")
          ;; save 
          (insert (wcy-escreen-save-session-variables (nth screen-counter wcy-escreen-obarray-list))))
    (insert (format
             "(setq wcy-escreen-current-obarray (nth %d 
            wcy-escreen-obarray-list))\n" wcy-escreen-current-screen-number))
    (insert (format 
             "(setq wcy-escreen-current-screen-number %d)\n"
             wcy-escreen-current-screen-number))
    (insert (format
             "(setq wcy-escreen-last-screen-number %d) \n"
             wcy-escreen-last-screen-number))
    (insert (format 
             "(setq wcy-escreen-mode-line-format %S)"
             wcy-escreen-mode-line-format))
    (insert "(run-hooks 'wcy-escreen-after-goto-screen-hook)")))
               
;;;; !!!!!!!!!!!!!!!!! the following functions to restore and save the screen data !!!!!!!!!!!
;;;;
;; ------------------ windows configuration -----------------

(defun wcy-escreen-map-save-window-configuration ()
  (wcy-escreen-set-local-variable "window-configuration" (current-window-configuration)))
(defun wcy-escreen-map-restore-window-configuration ()
  (let ((config (wcy-escreen-get-local-variable "window-configuration")))
    (and config (set-window-configuration config))))

(add-hook 'wcy-escreen-create-screen-hook 'wcy-escreen-map-save-window-configuration)
(add-hook 'wcy-escreen-before-goto-screen-hook 'wcy-escreen-map-save-window-configuration)
(add-hook 'wcy-escreen-after-goto-screen-hook 'wcy-escreen-map-restore-window-configuration)

;; ------------------- current buffer -----------------------
(defcustom escreen-new-screen-default-buffer "*scratch*"
  "*Default buffer to display in newly-created screens."
  :type 'string
  :group 'wcy-escreen)

(defcustom escreen-restore-killed-buffers nil
  "*If non-nil, automatically revisit files if they have been killed.
That is, if a buffer was killed while in another screen session,
recreate them, visiting whatever file they were visiting."
  :type 'boolean
  :group 'wcy-escreen)
(defun wcy-escreen-create-current-buffer()
  (switch-to-buffer (get-buffer-create escreen-new-screen-default-buffer))
  (wcy-escreen-save-current-buffer))
(defun wcy-escreen-save-current-buffer ()
  (wcy-escreen-set-local-variable "current-buffer"
                                  (mapcar 'funcall 
                                          `(current-buffer
                                            buffer-name
                                            buffer-file-name))))

(defun wcy-escreen-restore-current-buffer()
  (let ((data (wcy-escreen-get-local-variable "current-buffer" (current-buffer))))
    (let ((buffer (nth 0 data))
          (buffer-name (nth 1 data))
          (buf-file-name (nth 2 data)))
      (if (stringp buffer) (setq buffer (get-buffer buffer)))
      (cond ((escreen-killed-buffer-p buffer)
             (cond ((null escreen-restore-killed-buffers)
                    (set-window-buffer (selected-window)
                                       (get-buffer-create
                                        escreen-new-screen-default-buffer)))
                   ((stringp buf-file-name)
                    (setq buffer (find-file-noselect buf-file-name))
                    (set-window-buffer (selected-window) buffer)
                    (or (get-buffer buffer-name)
                        (rename-buffer buffer-name)))
                   (t
                    (set-window-buffer (selected-window)
                                       (get-buffer-create
                                        escreen-new-screen-default-buffer)))))
            (t
             (set-window-buffer (selected-window) buffer))))))

;; (defun wcy-escreen-save-session-current-buffer ()
;;   (let ((data (wcy-escreen-get-local-variable "current-buffer")))
;;     (format "(wcy-escreen-set-local-variable \"current-buffer\" (list (get-buffer %S) %S %S))\n"
;;             (nth 1 data)
;;             (nth 1 data)
;;             (nth 2 data))))

(add-hook 'wcy-escreen-create-screen-hook 'wcy-escreen-create-current-buffer)
(add-hook 'wcy-escreen-before-goto-screen-hook 'wcy-escreen-save-current-buffer)
(add-hook 'wcy-escreen-after-goto-screen-hook 'wcy-escreen-restore-current-buffer)


;; ------------------------- buffer list ----------------------------------------
(defun escreen-killed-buffer-p (buffer)
  (not (if (fboundp 'buffer-live-p)
           (buffer-live-p buffer)
         ;; Emacs 18 doesn't have buffer-live-p.
         ;; Killed buffers have no names.
         (buffer-name buffer))))

(defun wcy-escreen-save-buffer-list ()
  (wcy-escreen-set-local-variable "buffer-list" (buffer-list)))
(defun wcy-escreen-restore-buffer-list ()
  (let ((olist (wcy-escreen-get-local-variable "buffer-list" nil))
        firstbuf buf)
    (while olist
      (setq buf (car olist))
      (and (stringp buf)
           (setq buf (get-buffer buf)))
      (cond ((escreen-killed-buffer-p buf))
            (t
             (bury-buffer buf)
             (or firstbuf
                 (setq firstbuf buf))))
      (setq olist (cdr olist)))
    (setq olist (buffer-list))
    (while (not (eq (car olist) firstbuf))
      (bury-buffer (car olist))
      (setq olist (cdr olist)))))
(defun wcy-escreen-save-session-buffer-list()
  (let ((olist (wcy-escreen-get-local-variable "buffer-list" nil)))
    (format 
     " (wcy-escreen-set-local-variable \"buffer-list\" 
                                (list %s )) \n"
     (apply 'concat 
            (mapcar 
             (function (lambda (b) 
                         (format "%S " (buffer-name b)))) olist)))))
 
(add-hook 'wcy-escreen-create-screen-hook 'wcy-escreen-save-buffer-list)
(add-hook 'wcy-escreen-before-goto-screen-hook 'wcy-escreen-save-buffer-list)
(add-hook 'wcy-escreen-after-goto-screen-hook 'wcy-escreen-restore-buffer-list)


;; ---------------------------------------- screen name ----------------------------------------
(defun wcy-escreen-set-screen-name (name)
  (interactive "sPlease input the screen name:")
  (wcy-escreen-set-local-variable "screen-name" name)
  (wcy-escreen-set-mode-line))


(defun wcy-escreen-get-screen-name ()
  (interactive)
  (wcy-escreen-get-local-variable "screen-name" "*no name*"))

;; ---------------------------------------- mode line -----------------------------------------

(defun wcy-escreen-set-mode-line ()
  (setq wcy-escreen-mode-line-format 
        (format " S:%d[%d] / %d <%s> " 
                wcy-escreen-current-screen-number
                wcy-escreen-last-screen-number
                (1- (wcy-escreen-number-of-screen))
                (wcy-escreen-get-screen-name))))

(add-hook 'wcy-escreen-after-goto-screen-hook
          'wcy-escreen-set-mode-line)

;; ---------------------------------------- one screen per frame -------------------------------

(defun wcy-escreen-set-frame-screen-name (frame)
  (modify-frame-parameters frame `((name . ,(wcy-escreen-get-screen-name)))))
(defun wcy-escreen-create-frame ()
  (let ((f (new-frame)))
    (wcy-escreen-set-frame-screen-name f)
    (wcy-escreen-set-local-variable "frame" f)))
(defun wcy-escreen-restore-frame ()
  (let ((f (wcy-escreen-get-local-variable "frame")))
    (when (or (not f) (not (frame-live-p f)))
      (setq f (new-frame))
      (wcy-escreen-set-local-variable "frame" f))
    (wcy-escreen-set-frame-screen-name f)
    (select-frame-set-input-focus f)))
(defun wcy-escreen-kill-frame()
 (let ((f (wcy-escreen-get-local-variable "frame")))
   (when (frame-live-p f)
     (delete-frame f))))
(defun wcy-escreen-on-switch-frame(e)
  (interactive "e")
  (handle-switch-frame e)
  (let (c)
    (wcy-escreen-map-fun-screen 
     (lambda ()
       (if (eq (wcy-escreen-get-local-variable "frame" nil)
               (selected-frame))
           (setq c screen-counter))))
    (if (and c (not (eq c wcy-escreen-current-screen-number)))
        (wcy-escreen-goto-screen c))))
(defvar wcy-escreen-enable-multi-frame nil
  "enable frame per screen feature or not")
(when wcy-escreen-enable-multi-frame
  (global-set-key (kbd "<switch-frame>") 'wcy-escreen-on-switch-frame)
  (add-hook 'wcy-escreen-create-frame 'wcy-escreen-create-frame)
  (add-hook 'wcy-escreen-after-goto-screen-hook 'wcy-escreen-restore-frame)
  (add-hook 'wcy-escreen-kill-screen-hook 'wcy-escreen-kill-frame))
  
(provide 'wcy-escreen)
;;; wcy-escreen.el ends here
