;; $Id: smb-mode.el,v 1.3 1999/12/10 18:22:15 weare Exp weare $
;; smb-mode.el, Emacs Major Mode for editing smb.conf
;; v0.21
;; Author: Johnny Weare <jrweare@gmail.com> ; http://jrweare.googlepages.com
;; Original by: Fraser McCrossan <fraserm@gtn.net>
;; See http://jrweare.googlepages.com/smbmode.html for installation instructions
;; Last updated: 16-Dec-1998
;; Copyright (C) 1998-2006 Johnny Weare
;; Copyright (C) 1998 Fraser McCrossan
;; Portions copied from the "man.el" package by Barry A. Warsaw
;; <bwarsaw@cen.com> and others which is Copyright (C) 1993, 1994, 1996,
;; 1997 Free Software Foundation, Inc.

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defalias 'smb-backward-section 'backward-page)
(defalias 'smb-forward-section 'forward-page)
(defalias 'smb-narrow-to-section 'narrow-to-page)

(defvar smb-mode-map nil
  "Keymap for smb-mode.")

(defvar smb-menu-map nil
  "Menu for SMB Mode.")

(defvar smb-sec-menu-map nil
  "Menu for SMB Mode Sections.")

(defvar smb-mode-hooks nil
  "Hooks called when entering smb-mode.")

(defvar smb-complete-alist nil
  "Alist used for parameter completion in smb-mode; normally generated
automagically from the man page.")

(defvar smb-parameq-regex "^[ ;]*\\([^ ].*[^ ]\\) *="
  "Regular expression, subexpression 1 of which matches the parameter on
a line with a following \"=\".")

(defvar smb-paramnoeq-regex "^[ ;]*\\([^ =]\\([^=]*[^ =]\\)?\\)"
  "Regular expression, subexpression 1 of which matches the parameter on
a line with or without a following \"=\".")

(defvar smb-indent 3
  "*Spaces for standard indent.")

(defvar smb-comment-str ";"
  "*String to used to comment out a line.")

(defvar smb-pad-equal t
  "*If set, \"=\" characters are padded with spaces on either side if not already.")

(defvar smb-manual-file nil
  "*If set to a string, name of the file in which the smb.conf manual page can
be found. The file is assumed to have been cleaned up by removing backspaces,
underscores, and suchlike. If not a string, manual page is obtained by
running \"man smb.conf\".")

(defvar smb-use-outline-mode nil
  "*If non-nil, enter outline-minor-mode, using [sections] as headers. See
also \"smb-hide-immediate\". Also adds show/hide functionality to
\\[indent-for-tab-command] when on a section header.")

(defvar smb-hide-immediate nil
  "*If non-nil, and smb-use-outline-mode is non-nil, hide everything but
section headers immediately after entering outline-minor-mode.")

(defvar smb-manpage-buffer nil
  "Buffer containing the smb.conf manpage.")

(if smb-menu-map
    nil
  (setq smb-menu-map (make-sparse-keymap "SMB"))
  (define-key smb-menu-map [complete-param]
    '("Complete Parameter" . smb-complete-param))
  (define-key smb-menu-map [man-param]
    '("Parameter Manual Entry" . smb-man-param))
  (define-key smb-menu-map [toggle-yn-param]
    '("Toggle yes/no Parameter" . smb-toggle-yn-param))
  (define-key smb-menu-map [param-y]
    '("Set yes/no Parameter to Yes" . smb-param-y))
  (define-key smb-menu-map [param-n]
    '("Set yes/no Parameter to No" . smb-param-n))
  (define-key smb-menu-map [run-testparm]
    '("Run testparm on buffer" . smb-run-testparm))
  )

(if smb-sec-menu-map
    nil
  (setq smb-sec-menu-map (make-sparse-keymap "SMB-Section"))
  (define-key smb-sec-menu-map [widen]
    '("Un-narrow" . widen))
  (define-key smb-sec-menu-map [narrow-to-section]
    '("Narrow to" . smb-narrow-to-section))
  (define-key smb-sec-menu-map [kill-section]
    '("Kill" . smb-kill-section))
  (define-key smb-sec-menu-map [comment-section]
    '("Comment Out" . smb-comment-section))
  (define-key smb-sec-menu-map [new-section]
    '("New" . smb-new-section))
  )

(if smb-mode-map
    nil
  (setq smb-mode-map (make-sparse-keymap))
  (define-key smb-mode-map [menu-bar smb-sec]
    (cons "SMB-Section" smb-sec-menu-map))
  (define-key smb-mode-map [menu-bar smb]
    (cons "SMB" smb-menu-map))
  (define-key smb-mode-map "\C-x[" 'smb-backward-section)
  (define-key smb-mode-map "\C-x]" 'smb-forward-section)
  (define-key smb-mode-map "\C-xns" 'smb-narrow-to-section)
  (define-key smb-mode-map "]" 'smb-electric-bracket)
  (define-key smb-mode-map "=" 'smb-electric-equal)
  (define-key smb-mode-map "\M-\t" 'smb-complete-param)
  (define-key smb-mode-map "\C-cm" 'smb-man-param)
  (define-key smb-mode-map "\C-cr" 'smb-run-testparm)
  (define-key smb-mode-map "\C-ct" 'smb-toggle-yn-param)
  (define-key smb-mode-map "\C-cy" 'smb-param-y)
  (define-key smb-mode-map "\C-cn" 'smb-param-n)
  (define-key smb-mode-map "\C-c\C-s" 'smb-which-section)
  (define-key smb-mode-map "\C-c\C-w" 'smb-kill-section)
  (define-key smb-mode-map "\C-c\C-n" 'smb-new-section)
  (define-key smb-mode-map "\C-c\C-c" 'smb-comment-section)
  (define-key smb-mode-map "\C-c\C-tr" 'smb-cot-read-only)
  (define-key smb-mode-map "\C-c\C-tp" 'smb-cot-public)
  (define-key smb-mode-map "\C-c\C-tb" 'smb-cot-browseable)
  (define-key smb-mode-map "\C-c\C-to" 'smb-cot-printable))

(defun smb-which-section ()
  "Indicate the current smb.conf section."
  (interactive)
  (save-match-data
    (save-excursion
      (if (re-search-backward "^[ \t]*\\(\\[.+\\]\\)" nil t)
          (if (and
               (>= (match-beginning 1) (window-start))
               (<= (match-beginning 1) (window-end)))
              (progn
                (goto-char (match-beginning 1))
                (sit-for 1))
            (message (concat "In section " (match-string 1))))))))

(defun smb-previous-line-indent ()
  "Returns a string of the indentation of the previous line, or empty
string if there is no previous line."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (if (zerop (forward-line -1))
            (progn
              (end-of-line)
              (let ((limit (point)))
                (beginning-of-line)
                (if (re-search-forward "^[ \t]+" limit t)
                    (match-string 0)
                  "")))
          "")))))

(defun smb-line-is-something-p (re)
  "Generic line identifier."
  (save-excursion
    (end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (re-search-forward re limit t))))

(defun smb-line-is-comment-p ()
  "Return t if current line is a comment."
  (smb-line-is-something-p "\\(^[ \t]*\\)[;#]"))

(defun smb-line-is-section-p ()
  "Return t if current line is a section header."
  (smb-line-is-something-p "^\\([ \t]*\\[\\).+\\]"))

(defun smb-line-is-setting-p ()
  "Return t if current line is a setting (i.e. something = somethingelse)."
  (smb-line-is-something-p "^\\([ \t]*\\)[^[;]+"))

(defun smb-indent-line-flash ()
  "Indent line correctly in smb-mode, and flash."
  (interactive "*")
  (smb-indent-line t))

(defun smb-indent-line (&optional flash)
  "Indent line correctly in smb-mode, and optionally flash cursor to section.

If line is a section header, e.g. [homes], jam left.
If comment (beginning with ; or #), indent to previous line.
Otherwise, indent by value of smb-indent."
  (interactive "*")
  (save-excursion
    (end-of-line)
    (let* ((limit (make-marker))
           (limit (point)))
      (or
       (progn
         (if (smb-line-is-section-p)
             (progn
               (if (equal (match-string 1) "[")
                   nil
                 (replace-match "[" t nil nil 1))
               (if smb-use-outline-mode
                   (save-excursion
                     (end-of-line)
                     (if (outline-visible)
                         (hide-entry)
                       (show-entry)))
                 (setq flash nil))
               t)))
       (progn
         (if (smb-line-is-comment-p)
             (if (equal (match-string 1)
                        (smb-previous-line-indent))
                 nil
               (replace-match (smb-previous-line-indent) t nil nil 1)
               t)))
       (progn
         (if (smb-line-is-setting-p)
             (if (= (- (match-end 1) (match-beginning 1)) smb-indent)
                 nil
               (delete-region (match-beginning 1) (match-end 1))
               (beginning-of-line)
               (insert-char ?\  smb-indent)
               t))))))
  (if flash
      (smb-which-section)))

(defun smb-electric-bracket ()
  "Insert a ] symbol then indent line."
  (interactive "*")
  (insert "]")
  (smb-indent-line))

(defun smb-electric-equal ()
  "Insert an = symbol, pad it with spaces if necessary, then indent line."
  (interactive "*")
  (if (= (preceding-char) ?\ )
      nil
    (if smb-pad-equal
        (insert " ")))
  (insert "=")
  (if (= (following-char) ?\ )
      nil
    (if smb-pad-equal
        (insert " ")))
  (smb-indent-line))

(defun smb-new-section (newname)
  "Insert a new copy of the current section after the current section."
  (interactive "*MCopy this section as name: ")
  (next-line 1) ;; hack to make sure if point on section header, doesn't use
  ;; previous section
  (smb-backward-section)
  (beginning-of-line)
  (next-line 1)
  (let ((beg (point)))
    (smb-forward-section)
    (beginning-of-line)
    (save-excursion
      (insert (concat "[" newname "]\n" (buffer-substring beg (point)))))))

(defmacro smb-something-section (function)
  "Select the current section, then execute the region function \"function\"
on it."
  `(save-excursion
     (next-line 1) ;; hack to make sure if point on section header, doesn't use
     ;; previous section
     (smb-backward-section)
     (beginning-of-line)
     (let ((beg (point)))
       (smb-forward-section)
       (beginning-of-line)
       (,function beg (point)))))

(defun smb-kill-section ()
  (interactive "*")
  (smb-something-section kill-region))

(defun smb-comment-section ()
  (interactive "*")
  (smb-something-section comment-region))

;; this is the function copied from man.el
(defun smb-cleanup-manpage ()
  "Remove overstriking and underlining from the current buffer. Stolen in broad
daylight from the \"man\" package's Man-cleanup-manpage."
  (interactive)
  (message "Please wait: cleaning up the smb.conf manpage...")
  (goto-char (point-min))
  (while (search-forward "_\b" nil t) (backward-delete-char 2))
  (goto-char (point-min))
  (while (search-forward "\b_" nil t) (backward-delete-char 2))
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\)\\(\b\\1\\)+" nil t)
    (replace-match "\\1"))
  (goto-char (point-min))
  (while (re-search-forward "\e\\[[0-9]+m" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "o\b\\+\\|\\+\bo" nil t) (replace-match "o"))
  (goto-char (point-min))
  (while (re-search-forward "[-|]\\(\b[-|]\\)+" nil t) (replace-match "+"))
  (message "smb.conf man page cleaned up"))

(defun smb-toggle-yn-param (&optional yn)
  "Toggle the yes/no parameter under the cursor.

If optional argument YN is positive, set it to yes, otherwise no."
  (interactive "*P")
  (save-excursion
    (end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (if (not (re-search-forward "[ \t]*=[ \t]*\\([yY][eE][sS]\\|[nN][oO]\\) *$"
                                  limit t))
          (error "No yes/no parameter on the current line")
        (replace-match
         (if yn
             (if (> (prefix-numeric-value yn) 0)
                 "yes"
               "no")
           (if (equal (downcase (match-string 1)) "yes")
               "no"
             "yes"))
         t t nil 1)))))

(defun smb-param-y ()
  "Set a yes/no parameter to yes."
  (interactive "*")
  (smb-toggle-yn-param 1))

(defun smb-param-n ()
  "Set a yes/no parameter to no."
  (interactive "*")
  (smb-toggle-yn-param 0))

(defun smb-create-or-toggle (pname)
  "If supplied parameter doesn't exist in the current section, create it set
to \"yes\" otherwise toggle it."
  (save-excursion
    (smb-forward-section)
    (let ((limit (point))
          (re (concat "^[ \t]*" pname "[ \t]*=[ \t]*\\([^ \t\n]+\\)")))
      (smb-backward-section)
      (if (re-search-forward re limit t)
          (replace-match (if (equal (match-string 1) "yes")
                             "no"
                           "yes")
                         nil nil nil 1)
        (goto-char limit)
        (beginning-of-line)
        (re-search-backward "[^ \t\n]" nil t)
        (end-of-line)
        (insert (concat "\n" pname " = yes"))
        (smb-indent-line)))))

(defun smb-cot-read-only ()
  "Toggle current section's read only setting."
  (interactive "*")
  (smb-create-or-toggle "read only"))

(defun smb-cot-public ()
  "Toggle current section's public setting."
  (interactive "*")
  (smb-create-or-toggle "public"))

(defun smb-cot-browseable ()
  "Toggle current section's browseable setting."
  (interactive "*")
  (smb-create-or-toggle "browseable"))

(defun smb-cot-printable ()
  "Toggle current section's printable setting."
  (interactive "*")
  (smb-create-or-toggle "printable"))

;; I'm doing this myself because the standard man package doesn't return
;; anything useful to tell what buffer it's in
(defun smb-select-manpage ()
  "Select the buffer containing the smb.conf manual page. Create the buffer
and fetch the manpage if it doesn't exist yet."
  (end-of-line)
  (if (buffer-live-p smb-manpage-buffer)
      (set-buffer smb-manpage-buffer)
    (setq smb-manpage-buffer
          (get-buffer-create "*SMB Mode manual page*"))
    (set-buffer smb-manpage-buffer)
    (message "Please wait: fetching the smb.conf manpage...")
    (if (stringp smb-manual-file)
        (insert-file smb-manual-file)
      (call-process "man" nil t nil
                    "smb.conf")
      (smb-cleanup-manpage))
    (view-mode 1)))

(defun smb-man-param ()
  "Jump to the definition of the parameter under the cursor in the smb.conf
manual page. Call smb-select-manpage to ensure the manpage exists."
  (interactive)
  (let ((parameter ""))
    (save-excursion
      (end-of-line)
      (let ((limit (point)))
        (beginning-of-line)
        ;; find the current option
        (if (not (re-search-forward smb-parameq-regex limit t))
            (error "No parameter under cursor")
          ;; and if found, open up the manpage...
          (setq parameter (match-string 1))
          (smb-select-manpage)
          ;; find the parameter
          (goto-char (point-min))
          (let ((srch (concat "^ *" parameter " ?([A-Za-z])")))
            (if (not (re-search-forward srch nil t))
                (error "\"%s\" not found in manpage" parameter)
              (display-buffer smb-manpage-buffer)
              (set-window-start (get-buffer-window smb-manpage-buffer)
                                (match-beginning 0)))))))))

(defun smb-complete-param ()
  "Complete the parameter on the current line."
  (interactive "*")
  (let ((parameter ""))
    (end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (if (not (re-search-forward smb-paramnoeq-regex limit t))
          (error "No parameter under cursor")
        (smb-make-complete-alist)       ; builds it if it doesn't exist
        (let* ((ms (match-beginning 1))
               (me (match-end 1))
               (matchlist (all-completions (match-string 1)
                                           smb-complete-alist)))
          (if (not matchlist)
              (error "No parameter \"%s\"" (match-string 1))
            (let ((common (try-completion (match-string 1)
                                          smb-complete-alist)))
              (if (stringp common)
                  (progn
                    (delete-region ms me)
                    (insert common)))
              (if (> (length matchlist) 1)
                  (let ((msg nil))
                    (while matchlist
                      (setq msg (if msg
                                    (concat msg ";" (car matchlist))
                                  (concat (number-to-string (length matchlist))
                                          " matches:" (car matchlist))))
                      (setq matchlist (cdr matchlist)))
                    (error msg))
                (message "Complete!")))))))))

(defun smb-make-complete-alist ()
  "Build the parameter completion alist from the manual page."
  (interactive)
  (if smb-complete-alist
      nil
    (message "Please wait: building completion list from manual page...")
    (save-match-data
      (save-excursion
        (smb-select-manpage)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "^ *\\([^ \n].*[^ ]\\) ?([a-zA-Z])" nil t)
            (setq smb-complete-alist (cons `(,(match-string 1)) smb-complete-alist))))))))

(defun smb-run-testparm ()
  "Run the buffer contents through \"testparm\" and display output."
  (interactive)
                                        ; make a temp filename and get a buffer
  (let ((fname (make-temp-name "smbtmp"))
        (buffer (get-buffer-create "*SMB testparm results*")))
    (save-restriction
                                        ; undo a narrow (if any)
      (widen)
                                        ; write the buffer to the temp file
      (write-region (point-min) (point-max) fname)
                                        ; switch to the new buffer and erase it
      (save-excursion
        (set-buffer buffer)
        (erase-buffer)
                                        ;run testparm on this file, and output to the buffer up there
        (call-process "testparm" nil buffer nil fname)
                                        ; remove the temp file
        (delete-file fname)
                                        ; and finally display the results
        (goto-char (point-min))
        (display-buffer buffer)))))

;; font stuff
(defvar smb-font-lock-keywords
  (list
   '("^\\(\[[a-zA-Z0-9]*\]\\)"         1 font-lock-keyword-face        t t)
   '("^[ ]*\\([a-zA-Z0-9 ]+\\)[ ]*="   1 font-lock-variable-name-face  t t)
   '("\\(\"[^\"\n]+\"\\)"              1 font-lock-string-face         t t)
   '("\\(;[^\n]*\\)"                   1 font-lock-comment-face        t t))
  "Expressions to highlight in Samba mode.")


(defun smb-mode ()
  "Major Mode for editing Samba's smb.conf file. Simple support for font locking.
\\[smb-electric-bracket] indents electrically

\\[smb-electric-equal] indents electrically and does automatic SPC-padding
if smb-pad-equal is t.

\\[indent-for-tab-command] forces an auto-indent, and either flashes the
current section or shows it in the minibuffer.

The variable smb-indent controls indent level.

section-headers] jam left.
   setting = settings indent
   ;comments indent the same as the previous line

Mode specific commands:
\\{smb-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'smb-mode)
  (setq mode-name "SMB")
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'page-delimiter)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'outline-regexp)
  (setq paragraph-start "\\[\\|[ \t\n\^L]")
  (setq paragraph-separate "\\[.+\\]$\\|[ \t\n\^L]*$")
  (setq page-delimiter "^\\[.+\\]$")
  (setq indent-line-function 'smb-indent-line-flash)
  (setq comment-start smb-comment-str)
  ;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(smb-font-lock-keywords))
  (setq outline-regexp "^ *\\[.*\\]")
  (if smb-use-outline-mode
      (progn
        (outline-minor-mode)
        (if smb-hide-immediate
            (progn
              (hide-body)
              (message "WARNING: outline mode, only headers showing")))))
  (use-local-map smb-mode-map)
  (run-hooks 'smb-mode-hooks))

(provide 'smb)
