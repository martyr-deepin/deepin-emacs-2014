;;; eiv.el --- emacs image viewer

;; Copyright (C) 2008, 2009 Thierry Volpiatto
;; Author:     Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto 
;; Keywords: image, picture

;; Created: lun fév  2 11:38:44 2009 (+0100)
;; Last-Updated: mer fév  4 08:03:56 2009 (+0100)
;;           By: thierry
;;     Update #: 14

;; X-URL: http://freehg.org/u/thiedlecques/emacs-image-viewer 

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Commentary: 

;; Dependencies: - traverselisp.el:
;;                 http://www.emacswiki.org/cgi-bin/emacs/traverselisp.el
;;               - the ImageMagick package that provide "mogrify":
;;                 http://www.imagemagick.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl)
(require 'traverselisp)

;;;###autoload
(defun eiv-fit-image-to-window (arg)
  "Resize image to current window size.
With prefix arg don't preserve the aspect ratio."
  (interactive "P")
  (lexical-let ((cur-fname-to-resize
                 (buffer-file-name (current-buffer))))
    (let* ((edges (window-inside-pixel-edges))
           (width (- (nth 2 edges) (nth 0 edges)))
           (height (- (nth 3 edges) (nth 1 edges))))
      (apply #'start-process "resize-image" nil "mogrify"
             (list "-resize"
                   (concat (format "%dx%d" width height)
                           (and arg "!"))
                   cur-fname-to-resize))
      (set-process-sentinel (get-process "resize-image")
                            #'(lambda (process event)
                                (quit-window t)
                                (view-file cur-fname-to-resize)
                                (message "Ok %s on %s %s"
                                         process
                                         (file-name-nondirectory
                                          cur-fname-to-resize)
                                         event))))))

;;;###autoload
(defun eiv-rotate-current-image (&optional num-arg)
  "Rotate current image at 90 degrees.
with prefix arg at -90 degrees"
  (interactive)
  (let ((fname (buffer-file-name (current-buffer))))
    (unless num-arg
      (setq num-arg (if current-prefix-arg
                        -90
                        90)))
    (shell-command (format "mogrify -rotate %s %s" num-arg fname))
    (quit-window t)
    (view-file fname)))

;;;###autoload
(defun* eiv-diaporama (tree &optional (delay 2) (ext ".jpg"))
  (interactive "DTree: ")
  (cond ((equal current-prefix-arg '(4))
         (setq delay (read-number "Delay: ")))
        ((equal current-prefix-arg '(16))
         (setq ext (read-string "Ext: ")))
        ((equal current-prefix-arg '(64))
         (setq delay (read-number "Delay: "))
         (setq ext (read-string "Ext: "))))
  (traverse-apply-func-on-files
   tree
   #'(lambda (x)
       (view-file x)
       (sit-for delay)
       (quit-window t))
   ext))

;;;###autoload
(defun eiv-viewer (tree &optional only)
  "The emacs-image-viewer. Allow to navigate in a Tree of dir and subdir
of pictures. If prefix arg prompt for file ext to use.
By default use all files.
On each image, simple manipulations are possible:
- rotate left and right.
- resize image to window size.
NOTE: these manipulations are destructives on file
so when resizing you will be prompt to save image, if you DON'T save
your initial image will be LOST."
  (interactive (list (read-directory-name "DTree: " nil nil t)
                     (when current-prefix-arg
                       (read-string "OnlyExt: "))))
  (let* ((flist (traverse-list-files-in-tree tree nil only))
         (flist-iterator (tve-list-iterator flist))
         (action)
         (cur-elm)
         (flag-move))
    (catch 'break
      (while t
        (setq action (read-event "(n)ext (b)ack (q)uit (l)rotate-left (r)otate-right (f)it-to-window"))
        (case action
          ;; go forward
          ('?n
           (if (eq major-mode 'image-mode)
               (quit-window t))
           (if flag-move ;; direction changed we use a new iterator
               (let* ((fcur-pos (1+ (position cur-elm flist)))
                      ;; create a new list from pos to end of `flist'
                      (goforward-list (subseq flist fcur-pos))
                      (fnext-elm))
                 ;; refresh iterator from this new list
                 (setq flist-iterator (tve-list-iterator goforward-list))
                 (setq fnext-elm (tve-next flist-iterator))
                 (setq cur-elm fnext-elm)
                 (if fnext-elm
                     (view-file fnext-elm)
                     (throw 'break
                       (message "Finish! no more images"))))
               ;; Use initial iterator unless we change direction
               (let ((next-elm (tve-next flist-iterator)))
                 (setq cur-elm next-elm)
                 (if next-elm
                     (view-file next-elm)
                     (throw 'break
                       (message "Finish! no more images"))))))
          ;; go backward
          ('?b
           (if (eq major-mode 'image-mode)
               (quit-window t))
           (let* ((bcur-pos (position cur-elm flist))
                  ;; create a new list from pos to beg of `flist'
                  (goback-list
                   (reverse (subseq flist 0 bcur-pos)))
                  (bnext-elm))
             ;; refresh iterator from this new list
             (setq flist-iterator (tve-list-iterator goback-list))
             (setq bnext-elm (tve-next flist-iterator))
             (setq cur-elm bnext-elm)
             (setq flag-move t)
             (if bnext-elm
                 (view-file bnext-elm)
                 (throw 'break
                   (message "Finish! no more images")))))
          ;; rotate right
          ('?r
           (eiv-rotate-current-image))
          ;; rotate left
          ('?l
           (eiv-rotate-current-image -90))
          ;; resize to window
          ('?f
           (if (y-or-n-p "Save image?")
               (let* ((fname (buffer-file-name
                              (current-buffer)))
                      (tmp-fname (concat
                                  (symbol-name (gensym "img"))
                                  (file-name-extension fname t)))
                      (tmp-dname (concat default-directory
                                         "save/")))
                 (if (not (file-exists-p tmp-dname))
                     (make-directory tmp-dname)) 
                 (copy-file fname
                            (concat tmp-dname tmp-fname))
                 (eiv-fit-image-to-window nil))
               (eiv-fit-image-to-window nil)))
          ;; quit
          ('?q
           (quit-window t)
           (throw 'break nil)))))))


(provide 'eiv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eiv.el ends here
