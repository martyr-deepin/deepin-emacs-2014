;;; compile-dwim.el --- an interface to `compile'

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: compile-dwim.el,v 0.0 2007/12/08 04:48:42 ywb Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Features:
;; 1. Smart compile and run command for associated mode.
;; 2. File timestamp check both compiling and runing
;; 3. Easy for customization

;;; See also:
;; smart-compile.el by Seiji Zenitani <zenitani@mac.com>
;; smart-compile+.el by William XWL <william.xwl@gmail.com>

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'compile-dwim)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'format-spec)
(require 'compile)
(defvar compile-dwim-check-tools t
  "Whether checking makefile or ant or else.")

(defvar compile-dwim-cache nil
  "Last commands selected")

(defvar compile-dwim-alist
  `((perl (or (name . "\\.pl$")
              (mode . cperl-mode))
          "perl -wc \"%f\"" "perl \"%f\"")
    (c    (or (name . "\\.c$")
              (mode . c-mode))
          "gcc -o %n %f" ("./%n" "cint %f") "%n")
    (c++  (or (name . "\\.cpp$")
              (mode . c++-mode))
          "g++ -o %n %f" "./%n" "%n")
    ;; (haskell (or (name . "\\.hs$")
    ;;              (mode . haskell-mode))
    ;;          "ghc -XFlexibleContexts --make *.hs -o %n" "../bin/%n")
    (haskell (or (name . "\\.hs$")
                 (mode . haskell-mode))
             "runhaskell Setup configure --user && runhaskell Setup build && runhaskell Setup install" "./dist/build/%n/%n")
    (java (or (name . "\\.java$")
              (mode . java-mode))
          "javac %f" "java %n" "%n.class")
    (python (or (name . "\\.py$")
                (mode . python-mode))
            "python %f" "python %f")
    (javascript (or (name . "\\.js$")
                    (mode . javascript-mode))
                "smjs -f %f" "smjs -f %f")
    (tex   (or (name . "\\.tex$")
               (name . "\\.ltx$")
               (mode . tex-mode)
               (mode . latex-mode))
           "latex %f" "latex %f" "%n.dvi")
    (texinfo (name . "\\.texi$")
             "makeinfo %f" nil "%.info")
    (sh    (or (name . "\\.sh$")
               (mode . sh-mode))
           "/bin/sh ./%f" "/bin/sh ./%f")
    (f99   (name . "\\.f90$")
           "f90 %f -o %n" "./%n" "%n")
    (f77   (name . "\\.[Ff]$")
           "f77 %f -o %n" "./%n" "%n")
    (php   (or (name . "\\.php$")
               (mode . php-mode))
           "php %f" "php %f")
    (elisp (or (name . "\\.el$")
               (mode . emacs-lisp-mode)
               (mode . lisp-interaction-mode))
           (emacs-lisp-byte-compile) (emacs-lisp-byte-compile) "%fc"))
  "Settings for certain file type.
A list like ((TYPE CONDITION COMPILE-COMMAND RUN-COMMAND EXE-FILE) ...).
In commands, these format specification are available:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )
")

(defvar compile-dwim-run-buffer nil
  "Internal variable used by `compile-dwim-run'.
`compile-dwim-prompt-run' can't remember which buffer last used.")

(defsubst compile-dwim-conf (name conf)
  (nth (assoc-default name '((type . 0)
                             (condition . 1)
                             (compile . 2)
                             (run . 3)
                             (exe . 4))) conf))

(defsubst compile-dwim-spec ()
  (format-spec-make
   ?F (buffer-file-name)
   ?f (file-name-nondirectory (buffer-file-name))
   ?n (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
   ?e (file-name-extension (buffer-file-name))))

(defun compile-dwim-match-1 (buf filter)
  (cond ((eq (car filter) 'name)
         (and (buffer-file-name buf)
              (string-match (cdr filter) (buffer-file-name buf))))
        ((eq (car filter) 'mode)
         (eq (cdr filter) (buffer-local-value 'major-mode buf)))
        (t (error "Unimplement filter: %s" filter))))

(defun compile-dwim-match (buf filters)
  (cond ((eq (car filters) 'or)
         (let (result)
           (setq filters (cdr filters))
           (while filters
             (if (compile-dwim-match buf (car filters))
                 (setq result t
                       filters nil)
               (setq filters (cdr filters))))
           result))
        ((eq (car filters) 'not)
         (not (compile-dwim-match buf (cadr filters))))
        (t (not (null (compile-dwim-match-1 buf filters))))))

(defun compile-dwim-calculate-command (compile-p)
  (let ((alist compile-dwim-alist)
        match)
    (while alist
      (if (compile-dwim-match (current-buffer) (cadr (car alist)))
          (setq match (car alist)
                alist nil)
        (setq alist (cdr alist))))
    (when match
      ;; if the compile-command is setting by file variable or some
      ;; hook, the compile-dwim-cache should be empty and compile-command
      ;; should become local variable. so just return it
      (if (and (null compile-dwim-cache)
               (local-variable-p 'compile-command))
          (progn
            (set
             (make-local-variable 'compile-dwim-cache)
             `((compile . ,compile-command)
               (run . ,compile-command)))
            (cons (car match) (list compile-command)))
        (let ((cmds (compile-dwim-conf (if compile-p 'compile 'run) match))
              (spec (compile-dwim-spec))
              lisp-cmd)
          (setq cmds (delq nil (mapcar (lambda (cmd)
                                         (if (stringp cmd)
                                             (format-spec cmd spec)
                                           ;; if it is a symbol, make a funcall
                                           (setq lisp-cmd (if (listp cmd)
                                                              cmd (list cmd)))
                                           nil))
                                       ;; if it is a string or symbol, make a list
                                       (if (listp cmds)
                                           cmds (setq cmds (list cmds))))))
          (when (not lisp-cmd)
            ;; add makefile etc when compile
            (when (and compile-p compile-dwim-check-tools)
              (cond ((or (file-readable-p "Makefile")
                         (file-readable-p "makefile"))
                     (push "make" cmds))
                    ((file-readable-p "build.xml")
                     (push "ant" cmds))))
            ;; put history commands in compile-dwim-cache to top
            (setq cmds (delete-dups
                        (nconc
                         (delq nil
                               (mapcar (lambda (cmd)
                                         (if (eq (car cmd)
                                                 (if compile-p 'compile 'run))
                                             (cdr cmd)))
                                       compile-dwim-cache))
                         cmds))))
          (cons (car match) (or lisp-cmd cmds)))))))

;;;###autoload
(defun compile-dwim-compile (force &optional sentinel)
  (interactive "P")
  (if (not (buffer-file-name))
      (call-interactively 'compile)
    (let ((cmds (compile-dwim-calculate-command 'compile))
          match exe spec cancel)
      (if (null cmds)
          (call-interactively 'compile)
        (setq match (assoc (car cmds) compile-dwim-alist))
        (when (and (not force)
                   (setq exe (compile-dwim-conf 'exe match)))
          (setq spec (compile-dwim-spec)
                exe (format-spec exe spec))
          (when (and (file-exists-p exe)
                     (time-less-p (nth 5 (file-attributes (buffer-file-name)))
                                  (nth 5 (file-attributes exe))))
            (message "The exe file is newer! No need to compile!")
            (setq cancel t)))
        (when (not cancel)
          (setq cmds (cdr cmds))
          (if (null cmds)
              (message "No compile command found!")
            (if (stringp (car cmds))
                (progn
                  (setq compile-command (car cmds)
                        compile-history (nconc cmds compile-history))
                  (if sentinel
                      (add-hook 'compilation-finish-functions sentinel))
                  (call-interactively 'compile)
                  (make-local-variable 'compile-dwim-cache)
                  (add-to-list 'compile-dwim-cache
                               (cons 'compile compile-command)))
              (eval cmds)
              (if sentinel (funcall sentinel)))))))))

(defun compile-dwim-prompt-run (&rest ignore)
  ;; this function should call only once
  (remove-hook 'compilation-finish-functions 'compile-dwim-prompt-run)
  (when (yes-or-no-p "Compilation finished, run it now? ")
    (set-buffer compile-dwim-run-buffer)
    (compile-dwim-run)))

;;;###autoload
(defun compile-dwim-run ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'compile)
    (let ((cmds (compile-dwim-calculate-command nil))
          match exe spec cancel)
      (if (null cmds)
          (call-interactively 'compile)
        (setq match (assoc (car cmds) compile-dwim-alist))
        (when (setq exe (compile-dwim-conf 'exe match))
          (setq spec (compile-dwim-spec)
                exe (format-spec exe spec))
          (when (and (file-exists-p exe)
                     (time-less-p (nth 5 (file-attributes exe))
                                  (nth 5 (file-attributes (buffer-file-name)))))
            (setq cancel t)
            (when (yes-or-no-p "The exe file is expired, should we compile first? ")
              (setq compile-dwim-run-buffer (current-buffer))
              (compile-dwim-compile t 'compile-dwim-prompt-run))))
        (when (not cancel)
          (setq cmds (cdr cmds))
          (if (null cmds)
              (message "No compile command found!")
            (if (stringp (car cmds))
                (progn
                  (setq compile-command (car cmds)
                        compile-history (nconc cmds compile-history))
                  (call-interactively 'compile)
                  (make-local-variable 'compile-dwim-cache)
                  (add-to-list 'compile-dwim-cache
                               (cons 'run compile-command)))
              (eval cmds))))))))

(provide 'compile-dwim)
;;; compile-dwim.el ends here
