;;; workspaces.el --- virtual "workspaces" for emacs.

;; Author: Mark Triggs <mst@dishevelled.net>

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

;; I've been using escreen for quite a while to make it easy to jump between
;; different tasks.  Traditionally I've had one screen for Gnus, one screen
;; for IRC and another for whatever I'm editing.

;; Having only a single screen for editing was sometimes a bit limited, so
;; this code formalises the idea of "workspaces".  This code allows you to
;; create and delete workspaces, list all workspaces and the buffers that are
;; currently visible and switch between them.

;; This is really just a wrapper around Noah Friedman's escreen, so all credit
;; should go to Noah.

;; To use it, add something like the following to your ~/.emacs:

;;  (define-key global-map (kbd "C-<tab>") 'workspace-controller)

;; Then, hitting C-TAB will give a list of workspaces and a summary of
;; key-bindings (for example, "C-TAB 1" switches to workspace 1, "C-TAB TAB"
;; creates a new workspace).

;;; Code:

(require 'escreen)

(defvar workspaces '())
(defvar workspace-count 0)
(defvar workspace-controller-buffer "*workspace-controller*")
(defvar workspace-last-window-configuration nil)
(defvar workspace-current nil "The currently selected workspace")
(defvar workspace-last nil "The workspace that was selected before this one")
(defvar workspace-controller-map (make-sparse-keymap))


(defun workspace-visible-buffers ()
  (let ((buffers '()))
    (walk-windows
     (lambda (w) (push (window-buffer w) buffers)))
    buffers))


(defstruct workspace escreen)


(defun workspace-find (n)
  (if (and (< n workspace-count) (>= n 0))
      (nth (1- (- workspace-count n)) workspaces)
    nil))


(defun workspace-create ()
  "Create and switch to a new workspace."
  (interactive)
  (escreen-create-screen)
  (push (make-workspace :escreen escreen-current-screen-number)
        workspaces)
  (incf workspace-count)
  (setq workspace-last workspace-current)
  (setq workspace-current (1- workspace-count))
  (message "New workspace"))


(defun workspace-kill (&optional n)
  "Kill workspace N."
  (interactive
   (list (string-to-number
          (completing-read "Kill which workspace?: "
                           (loop for i from 0 below workspace-count
                                 collect (number-to-string i))))))
  (escreen-kill-screen (workspace-escreen (workspace-find n)))
  (setq workspaces (remove (workspace-find n) workspaces))
  (decf workspace-count)
  (cond ((and (= workspace-current n) (not (zerop workspace-count)))
         (workspace-select (1- workspace-count)))
        ((< n workspace-current) (decf workspace-current))
        (t nil)))


(defun workspace-select (n)
  (cond ((and (>= n 0) (< n workspace-count))
         (unless (= n workspace-current)
           (setq workspace-last workspace-current)
           (setq workspace-current n))
         (escreen-goto-screen (workspace-escreen (workspace-find n))))
        (t (message "No such workspace"))))


(defmacro workspace-with-screen (screen-number &rest body)
  (let ((oldscreen (gensym)))
    `(let ((,oldscreen escreen-current-screen-number))
       (escreen-goto-screen ,screen-number)
       (unwind-protect (progn ,@body)
         (escreen-goto-screen ,oldscreen)))))
(put 'workspace-with-screen 'lisp-indent-function 1)


(defun workspace-contents (n)
  (workspace-with-screen (workspace-escreen (workspace-find n))
    (remove workspace-controller-buffer
            (mapcar 'buffer-name (workspace-visible-buffers)))))


(defun workspace-kill-workspace-under-point ()
  (interactive)
  (let ((overlay (car (overlays-at (point)))))
    (if overlay
        (let ((workspace (overlay-get overlay 'workspace)))
          (workspace-kill workspace)
          (workspace-controller))
      (error "No workspace under point"))))


(defun workspace-find-next ()
  "Find and return the location of the next workspace line."
  (save-excursion
    (catch 'result
      (while (not (eobp))
        (next-line 1)
        (when (some (lambda (o) (overlay-get o 'workspace))
                    (overlays-at (point)))
          (throw 'result (point)))))))


(defun workspace-find-prev ()
  "Find and return the location of the previous workspace line."
  (save-excursion
    (catch 'result
      (while (not (bobp))
        (next-line -1)
        (when (some (lambda (o) (overlay-get o 'workspace))
                    (overlays-at (point)))
          (throw 'result (point)))))))


(defun workspace-next-workspace (&optional n)
  (interactive "p")
  (let ((count (or n 1)))
    (catch 'no-more
      (while (and (not (eobp))
                  (>= (decf count) 0))
        (let ((next (workspace-find-next)))
          (if next
              (goto-char next)
            (throw 'no-more nil)))))))


(defun workspace-previous-workspace (&optional n)
  (interactive "p")
  (let ((count (or n 1)))
    (catch 'no-more
      (while (and (not (bobp))
                  (>= (decf count) 0))
        (let ((prev (workspace-find-prev)))
          (if prev
              (goto-char prev)
            (throw 'no-more nil)))))))


(defun workspace-select-workspace-under-point ()
  (interactive)
  (let ((overlay (car (overlays-at (point)))))
    (if overlay
        (let ((workspace (overlay-get overlay 'workspace)))
          (workspace-controller-close)
          (workspace-select workspace))
      (error "No workspace under point"))))


(defun workspace-setup-bindings ()
  (define-key workspace-controller-map (kbd "q") 'workspace-controller-close)
  (define-key workspace-controller-map (kbd "RET")
    'workspace-select-workspace-under-point)
  (define-key workspace-controller-map (kbd "SPC")
    (lambda ()
      (interactive)
      (workspace-controller-close)
      (workspace-goto-last)))
  (define-key workspace-controller-map (kbd "n") 'workspace-next-workspace)
  (define-key workspace-controller-map (kbd "p") 'workspace-previous-workspace)
  (define-key workspace-controller-map (kbd "TAB")
    (lambda ()
      (interactive)
      (workspace-controller-close)
      (workspace-create)))
  (loop for i from 1 to 9 do
        (define-key workspace-controller-map (number-to-string i)
          `(lambda () (interactive)
             (workspace-controller-close)
             (workspace-select ,(1- i)))))
  (define-key workspace-controller-map (kbd "k")
    'workspace-kill-workspace-under-point))


(define-derived-mode workspace-controller-mode
  fundamental-mode "workspace controller"
  "The major mode for the workspace controller buffer"
  (setq buffer-read-only t)
  (use-local-map workspace-controller-map))


(defun workspace-controller-close ()
  "Close the workspace controller."
  (interactive)
  (let ((buffer (get-buffer workspace-controller-buffer)))
    (when buffer
      (with-current-buffer buffer
        (kill-buffer nil)
        (set-window-configuration workspace-last-window-configuration)))))


(defun workspace-controller ()
  "Display the workspace controller."
  (interactive)
  (workspace-controller-generate)
  (unless (eq (current-buffer) (get-buffer workspace-controller-buffer))
    (setq workspace-last-window-configuration (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer-other-window workspace-controller-buffer)))


(defun workspace-controller-generate ()
  (let ((workspace-contents
         (loop for i from 0 below workspace-count
               collect (workspace-contents i))))
    (with-current-buffer (get-buffer-create
                          workspace-controller-buffer)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))

      (insert (propertize "Workspace controller\n\n" 'face 'bold))

      (insert (propertize (format "Key bindings:\n") 'face 'bold))
      (insert "  [n]  -- select workspace N\n")
      (insert "  k    -- kill the workspace under point\n")
      (insert "  RET  -- select the workspace under point\n")
      (insert "  tab  -- create a new workspace\n")
      (insert "  q    -- kill this buffer\n")

      (insert (propertize "\nWorkspaces:\n" 'face 'bold))
      (let ((selection nil))
        (dotimes (workspace workspace-count)
          (let ((p (point)))
            (insert (format "%d: %s %s\n" (1+ workspace)
                            (nth workspace workspace-contents)
                            (cond ((and (= workspace-current workspace)
                                        (= escreen-current-screen-number
                                           (workspace-escreen
                                            (workspace-find workspace))))
                                   (setq selection (point))
                                   (propertize "(selected)" 'face 'bold))
                                  (t ""))))
            (overlay-put (make-overlay p (point))
                         'workspace workspace)))
        (when selection (goto-char selection)))
      (workspace-controller-mode))))


(defun workspace-goto-current ()
  (interactive)
  (if (not workspaces)
      (workspace-create)
    (workspace-select workspace-current)))

(defun workspace-goto-last ()
  "Switch back to the second most recent workspace"
  (interactive)
  (if (and (>= workspace-last 0) (< workspace-last workspace-count))
      (workspace-select workspace-last)
    (loop for i from (1- workspace-count) downto 0 do
          (when (/= i workspace-current)
            (workspace-select i)
            (return nil)))))


(workspace-setup-bindings)

(provide 'workspaces)
;;; workspaces.el ends here
