;;; arxiv-reader.el --- an interface for reading and sorting arXiv abstracts.
;;; Inspired by Hubert Chen's java "reader."

;;; Copyright (C) 2008 Peter H. Mao

;; Author: Peter H. Mao <peter.mao@gmail.com> <peterm@srl.caltech.edu>
;; Version: %Id: 2%
;; RCSVersion: 1.12

;; arxiv-reader.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version. (http://www.gnu.org/licenses/gpl-3.0.txt)

;; arxiv-reader.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; This package extends the functionality of look-mode.  It defines
;; the minor mode "arxiv" which provides a keymap to move abstracts
;; into and between subdirectories.  
;; 
;; Side effect: arXiv mode sets "look-show-subdirs" and to t.  This
;; makes look list the subdirectories of the starting directory on
;; the header line.
;; 
;; arxiv-move-to-subdir (bound to 'R') moves the currently shown
;; file into the 1st directory.  To move files into other
;; directories, use a numeric prefix argument.  Abstracts can be
;; moved back into the starting directory by using 0 as the prefix
;; argument.
;; 
;; arxiv-get-pdf (bound to 'P') gets the pdf of the current
;; abstract, puts it in <filename><Last><FI>.pdf and appends '.x' to
;; the abstract filename.  The pdf will be saved in the same
;; directory as the current file and will be opened using 'doc-view'.
;; 
;; The program typically is run from dired mode with "r" bound to
;; the function arxiv-read-abstracts and 'a' bound to
;; arxiv-look-at-abstracts.  See setup instructions below.

;;; Setup:
;;
;; put this file and look-mode.el into a directory in your load-path.
;; look-mode also requires eimp.el, but that should be fixed at some point.
;; Or cons them onto your load-path.
;; ex: (setq load-path (cons "~/my_lisp_files/" load-path))
;;     (load "look-mode")
;;     (load "arxiv-reader")
;; I like to bind "r" to arxiv-read-abstracts in dired
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key dired-mode-map "r" 'arxiv-read-abstracts)))
;;             (define-key dired-mode-map "a" 'arxiv-look-at-abstracts)))

;;; Usage:
;;
;; (arxiv-read-abstracts &optional filename)
;;
;; in look-mode, you can run (arxiv-move-to-subdir subdir-number) where
;; subdir-number is the integer in front of the directory name.  With
;; no subdir-number specified, the program uses 1.

;;; future:
;;
;; pdf download, run acroread or whatever for reading pdfs

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variables and definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup arxiv nil
  "A mode for reading arXiv abstracts"
  :prefix "arxiv-"
  :group 'applications)

(defcustom arxiv-keyword-list nil
  "A list of regexp keywords to highlight in arXiv abstracts."
  :group 'arxiv
  :type '(repeat regexp))

(defvar arxiv-minor-mode-map
  (let ((map (make-sparse-keymap)))
;    (define-key map (kbd "d") 'symbol);mark for deletion)
    (define-key map (kbd "R")   'arxiv-move-to-subdir)
    (define-key map (kbd "P")   'arxiv-get-pdf)
    (define-key map (kbd "C-.") 'arxiv-look-at-next-file)
    (define-key map (kbd "C-,") 'arxiv-look-at-previous-file)
    (define-key map (kbd "M-]") 'arxiv-look-at-next-file)
    (define-key map (kbd "M-[") 'arxiv-look-at-previous-file)
    (define-key map (kbd "C-c k") 
      (lambda ()
        (interactive)
        (customize-variable 'arxiv-keyword-list)))
    map)
  "Keymap for arXiv mode.")

(define-minor-mode arxiv-mode
  "a minor mode to read arXiv abstracts.  Defines keybindings to
  move files into subdirectories."
  :init-value nil ; maybe make this t?
  :lighter " arXiv"
  :keymap arxiv-minor-mode-map
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interactive functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arxiv-read-abstracts ()
  "call arxiv-split-abstracts then start look mode with arxiv mode"
  (interactive)
  (arxiv-split-abstracts)
  (arxiv-look-at-abstracts)
  )

(defun arxiv-look-at-abstracts ()
  "start look mode for reading abstracts.  This is used when the abstracts
have already been split from the mail file."
  (interactive)
  (setq look-show-subdirs t)
  ; I save the mail files as "newMDD" and I have a file called "notes"
  ; neither should be in the abstract list
  (add-to-list 'look-skip-file-list "^n[eo]")
  (add-to-list 'look-skip-file-list "pdf$")
  (look-at-files "")
  (arxiv-start-arxiv-mode)
  (pop look-skip-file-list)
  (pop look-skip-file-list)
  ) 

(defun arxiv-look-at-next-file ()
  "calls look-at-next-file.  will have some addons in the future"
  (interactive)
  (look-at-next-file)
  (arxiv-start-arxiv-mode)
  )

(defun arxiv-look-at-previous-file ()
  "calls look-at-previous-file.  will have some addons in the future"
  (interactive)
  (look-at-previous-file)
  (arxiv-start-arxiv-mode)
  )

(defun arxiv-move-to-subdir (subdir-number)
  "Move current file into the subdir-number^th directory in
   look-subdir-list.  With no explicit prefix, it puts the file
   in the first listed subdirectory."
  (interactive "p"); defaults to 1
  (let* ((arxiv-target-dir (nth subdir-number look-subdir-list))
         (arxiv-new-filename 
          (concat
           look-pwd ; (file-name-directory look-current-file)
           arxiv-target-dir
           (file-name-nondirectory look-current-file)))
         arxiv-match-string)
    (if (not (file-exists-p arxiv-new-filename))
        (progn 
          (rename-file look-current-file arxiv-new-filename)
          (princ (concat "Moved " look-current-file " to " arxiv-target-dir))
          (setq look-current-file arxiv-new-filename)
          (look-update-header-line)
          )
      ;see if  "replaced with ... \d+\w+)" appears in the existing file
      ;I'd like to do this more generally, but this works (for now)
      (beginning-of-buffer); the search is point-location sensitive
      (if (search-forward-regexp 
           "^\\(replaced [[:alnum:][:space:],:]+([[:alnum:],]+)\\)$" nil t)
          (progn 
            (setq arxiv-match-string (match-string 1))
            (beginning-of-buffer); for tidiness
            (switch-to-buffer "*arxiv-temp*")
            (insert-file-contents arxiv-new-filename)
            (if (search-forward arxiv-match-string nil t)
                (princ (concat look-current-file " has already been moved to "
                               arxiv-new-filename))
              (shell-command (concat "cat " look-current-file " >> "
                             arxiv-new-filename))
              (princ (concat "Appended " look-current-file " to "
                             arxiv-new-filename))
              )
            (kill-buffer "*arxiv-temp*")
            )
        (princ "File exists in subdirectory, but could not find a \"replaced with...\" string")
        ) ;fi
      ) ;fi
    ) ;tel
  ) ;nufed

(defun arxiv-get-pdf ()
  "download and display the pdf of the current file"
  (interactive)
  (beginning-of-buffer)
  ;  extract the first author name
  (search-forward-regexp 
;   "^Authors?: \\([[:alpha:]-'\"\\. ]+?\\)\\( ?(\\|,\\| et\\| and\\|$\\)")
   "^Authors?: \\(.*?\\)\\( ?(\\|,\\| et\\| and\\|$\\)")
  (beginning-of-buffer)
  (let* ( (first-author-name (match-string 1))
          first-initial
          last-name
          formatted-name
          (abstract-number (replace-regexp-in-string 
                            "\\(^arXiv:\\|\\.x$\\)" "" 
                            (file-name-nondirectory look-current-file)))
          (arxiv-url (concat "http://xxx.lanl.gov/pdf/" 
                             (replace-regexp-in-string
                              "\\([a-z]\\)\\([0-9]\\)" "\\1/\\2" ;reinsert the slash
                              abstract-number)))
          output-file
        )
    ;parse the first author name
    (string-match "^\\([A-Z]\\)" first-author-name)
    (setq first-initial (match-string 1 first-author-name))
    (string-match 
     ;   name prefixes                   |  last name  |   titles
     "\\(\\(\\(van\\|de\\)[[:space:]]+\\)?[[:alpha:]'\"\\]+\\([[:space:]]+[JS]r\\)?\\)\\.?$"
     first-author-name)
    (setq last-name (match-string 1 first-author-name))
    (setq formatted-name 
          (replace-regexp-in-string " " "_" (concat last-name first-initial)))
    (setq output-file (concat (file-name-directory look-current-file)
                              "arXiv:" abstract-number "_" formatted-name ".pdf"))
    ; get the pdf and change the abstract file name
    (if (file-regular-p (replace-regexp-in-string "\\\\" "" output-file))
        (princ (concat output-file " already exists"))
      ; get the file
      (shell-command (concat "curl -o " output-file " " arxiv-url))
      ; move the abstract to .x
      (unless (string-match "\\.x" look-current-file)
        (progn
          (rename-file look-current-file (concat look-current-file ".x"))
          (setq look-current-file (concat look-current-file ".x"))
          (look-update-header-line)
          ))
      )
    (princ (concat last-name first-initial))
    (doc-view nil output-file) ; or make a shell-command to your favorite pdf reader.
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutines (non interactive defun's) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arxiv-split-abstracts (&optional filename)
  "split an arXiv email into separate abstracts
taken shamelessly from Hubert Chen's 'breakup' perl script
that does exactly the same thing."
  (setq filename (dired-get-file-for-visit))
  (switch-to-buffer "*arxiv-extract-abstracts*")
  (insert-file-contents filename)
  (while (re-search-forward "\\(^arXiv:[-./[:alnum:]]+\\)" nil t)
    (setq abs-filename
          (replace-regexp-in-string "/" "" (match-string 0)))
    (move-beginning-of-line 1)
    (setq abs-start (point))
    (search-forward "\\ (" )
    (move-beginning-of-line 2)
    (setq abs-end (point))
    (write-region abs-start abs-end abs-filename nil nil nil)
    )
  (kill-buffer "*arxiv-extract-abstracts*")
  )  

(defun arxiv-start-arxiv-mode ()
  "starts arxiv minor mode and highlights keywords"
  (arxiv-mode t)
  (dolist (arxiv-re arxiv-keyword-list)
    (highlight-regexp arxiv-re))
  )
