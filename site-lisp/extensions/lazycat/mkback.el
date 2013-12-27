;;; mkback.el---advanced assistance to manual archiving/backup of files.                                       
;; Time-stamp: <2004-11-29 17:03:37 deego>                                                                     
;; Copyright (C) 2002 D. Goel                                                                                  
;; Copyright (C) 2002 Free Software Foundation, Inc.                                                           
;; Emacs Lisp Archive entry                                                                                    
;; Filename: mkback.el                                                                                         
;; Package: mkback                                                                                             
;; Author: Deepak Goel <deego@gnufans.org>                                                                     
;; Keywords: backup project                                                                                    
;; Version: 1.5dev                                                                                             
;; For latest version:                                                                                         

(defvar mkback-home-page                                                                                       
  "http://www.gnufans.net/~deego/emacspub/lisp-mine/fastron/")                                                 

;; Namespace: mkback-,                                                                                         

;; This file is NOT (yet) part of GNU Emacs.                                                                   

;; This is free software; you can redistribute it and/or modify                                                
;; it under the terms of the GNU General Public License as published by                                        
;; the Free Software Foundation; either version 2, or (at your option)                                         
;; any later version.                                                                                          

;; This is distributed in the hope that it will be useful,                                                     
;; but WITHOUT ANY WARRANTY; without even the implied warranty of                                              
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                               
;; GNU General Public License for more details.                                                                

;; You should have received a copy of the GNU General Public License                                           
;; along with GNU Emacs; see the file COPYING.  If not, write to the                                           
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,                                                
;; Boston, MA 02111-1307, USA.                                                                                 


;; uncoment this bash script, tweak if needed and save it to, say,                                             
;; ~/bin/mkback. From then on, commands like mkback * will work                                                
;; (interactively) from bash.                                                                                  

;;; #!/bin/bash                                                                                                

;;; emacs -nw -l ~/.emacs --eval="(require 'mkback)" \                                                         
;;; --eval="(require 'mkback)" \                                                                               
;;; --eval="(mkback-from-batch $*)"                                                                            

;; (with thanks to Damian Elmes), if you prefer aliases: (untested)                                            
;; alias mkback="emacs -batch -nw --eval=\"(progn (require 'mkback) (mkback-from-batch $*)\""                  


(eval-when-compile (require 'cl))                                                                              

;; Quick start:                                                                                                
(defvar mkback-quick-start                                                                                     
  "See M-x mkback-introduction.                                                                                
                                                                                                                 
  Drop mkback.el somewhere in your load-path and add to your .emacs.                                             
  \(require 'mkback\)                                                                                            
  \(mkback-install-for-eshell\)                                                                                  
                                                                                                                 
                                                                                                                 
  For advanced users who use autoload mkback, simply add this to .emacs                                          
  instead of the above:                                                                                          
  \(defvar mkback-after-load-hooks\)                                                                             
  \(add-hook 'mkback-after-load-hooks 'mkback-install-for-eshell\)                                               
                                                                                                                 
                                                                                                                 
  You should now have access to M-x mkback in emacs *and* in the                                                 
  command-line mkback in eshell.                                                                                 
                                                                                                                 
  Note that the mkback-install-for-eshell step is optional.  Mkback will                                         
  work in eshell even without this step, but this step makes it do good                                          
  things for eshell---see commentary.                                                                            
                                                                                                                 
  For bash access to mkback, see the bash script above.  Try the various                                         
  defcustoms to customize."  )                                                                                   

;;;###autoload                                                                                                 
(defun mkback-quick-start ()                                                                                   
  "Provides electric help regarding `mkback-quick-start'."                                                     
  (interactive)                                                                                                
  (with-electric-help                                                                                          
   '(lambda () (insert mkback-quick-start) nil) "*doc*"))                                                      

;;; Introduction:                                                                                              
;; Stuff that gets posted to gnu.emacs.sources                                                                 
;; as introduction                                                                                             
(defvar mkback-introduction                                                                                    
  "mkback searches for a backup/ folder in the                                                                 
  file's directory, or its parent directory, or the grandparent                                                  
  directory , and so on.  It then backs up the requested file with                                               
  a mirroring of the relative directory structure, and the                                                       
  current date/time information. The file in question need not be a text                                         
  file.                                                                                                          
                                                                                                                 
  The primary functions from emacs are M-x mkback and M-x mkback-buffer.                                         
  Add (mkback-install-for-eshell), and you have an eshell-optimized                                              
  command called mkback. Finally, you can use mkback from bash by                                                
  calling emacs in batch-mode, See the included batch-script at the top                                          
  of this file.                                                                                                  
                                                                                                                
 Only tested on GNU/Linux.  Designed in a platform-independent                                                  
 way--should even work on VMS.  Tested with Emacs21.2 only.Type M-x                                             
 mkback-quick-start and M-x mkback-commentary for more details.                                                 
 "  )                                                                                                           





;;;###autoload                                                                                                 
(defun mkback-introduction ()                                                                                  
  "Provides electric help regarding `mkback-introduction'."                                                    
  (interactive)                                                                                                
  (with-electric-help                                                                                          
   '(lambda () (insert mkback-introduction) nil) "*doc*"))                                                     

;;; Commentary:                                                                                                
(defvar mkback-commentary                                                                                      
  "Please M-x mkback-quick-start and M-x mkback-introduction first.                                            
                                                                                                                
 Optionally, add (mkback-install-for-eshell) to .emacs.  That makes                                             
 mkback do nice things---                                                                                       
                                                                                                                
 * in eshell, typing mkb TAB some-file-name ENTER works.                                                        
 * in eshell, typing mkb TAB ENTER works..                                                                      
                                                                                                                
 Note that this tab-completion may not work the very first time if you                                          
 follow the autoload-route to mkback-install-for-eshell.                                                        
                                                                                                                
 See the various defcustoms, hooks for customization.                                                           
                                                                                                                
                                                                                                                
 I periodically made backups of the files I used, annotating the                                                
 backups with today's dates.  Here are some design decisions:                                                   
                                                                                                                
 * The folder is called backup but customizable.                                                                
                                                                                                                
 * Sometimes, one does not want a folder to be cluttered by a huge                                              
   backup/.  Consider this: project/folder1/ project/folder2/ and                                               
   project/folder3/.  You often need to tar up your project/ to deliver                                         
   it to folks.  Now, you don't want folders like                                                               
   project/folder1/backup/ existing.                                                                            
                                                                                                                
   In such a case, you would rather mkback a file like                                                          
   project/folder1/file.lisp into                                                                               
   project/backup/folder1/file-date.lisp.                                                                       
                                                                                                                
   Thus, mkback looks in current folder and in ancestors for                                                    
   backup/'s.                                                                                                   
                                                                                                                
                                                                                                                
 * I did not want to name foo.lisp as foo.lisp-date, because that                                               
 changes extension, thus emacs etc. had a hard time recognizing the                                             
 backup-file's type. if i ever wanted to browse the backuped file.  So                                          
 I preferred foo-date.lisp                                                                                      
                                                                                                                
 * Most of the time, I would not make more than once backup in a day,                                           
 but if I did, I could call the new one foo-date-a.lisp etc.                                                    
                                                                                                                
 * If the date is listed in yyyy-mm-dd format, then an alphabetical                                             
 directory listing is \(mostly\) also a time-ordered directory listing.                                         
 Pretty convenient.  I have started naming all my dates in this                                                 
 format. One can customize the date-format.                                                                     
                                                                                                                
 * I am almost always in eshell when I do an archiving.  So, there we go..                                      
                                                                                                                
                                                                                                                
 If you are working on a patch or new feature, it is recommended that                                           
 you download the latest mkback from mkback-home-page first, and work                                           
 on that.                                                                                                       
                                                                                                                
                                                                                                                
 "                                                                                                              
  )                                                                                                              

;;;###autoload                                                                                                 
(defun mkback-commentary ()                                                                                    
  "Provides electric help regarding `mkback-commentary'."                                                      
  (interactive)                                                                                                
  (with-electric-help                                                                                          
   '(lambda () (insert mkback-commentary) nil) "*doc*"))                                                       

;;; History:                                                                                                   

;;; Bugs:                                                                                                      






;;; New features:                                                                                              
(defvar mkback-new-features                                                                                    
  "                                                                                                            
                                                                                                                
  New since 1.4                                                                                                 
  ============================================                                                                  
                                                                                                                
  * By default, the file-modification time of the backup-ed file is                                             
    now same as that of the original file.                                                                      
                                                                                                                
  * By default, The backuped file's name now stores both the                                                    
    file-modification-time as well as the time at which the backup is                                           
    made.                                                                                                       
                                                                                                                
                                                                                                                
                                                                                                                
 "                                                                                                              
  )                                                                                                              


;;;###autoload                                                                                                 
(defun mkback-new-features ()                                                                                  
  "Provides electric help regarding `mkback-new-features'."                                                    
  (interactive)                                                                                                
  (with-electric-help                                                                                          
   '(lambda () (insert mkback-new-features) nil) "*doc*"))                                                     

(defvar mkback-version "1.5dev")                                                                               

;;==========================================                                                                   
;;; Code:                                                                                                      


(defcustom mkback-chase-links-method 'dir                                                                      
  "How to chase symlinks                                                                                       
 This can take 4 values:                                                                                        
 'dir, 'file 'all and 'none.                                                                                    
 'file: only chase file links,                                                                                  
 'all: chase all links,                                                                                         
 'none: don't chase links,                                                                                      
 The author likes the 'dir option. ")                                                                           

(defvar mkback-before-load-hooks nil)                                                                          
(defvar mkback-after-load-hooks nil)                                                                           
(run-hooks 'mkback-before-load-hooks)                                                                          


(defcustom mkback-create-new-backup-dir-p nil "")                                                              
(defcustom mkback-dir "backup" "")                                                                             

(defcustom mkback-time-format                                                                                  
  "-%Y%m%d-%H%M-%S"                                                                                            
  "The string to use for time-format..  More generally, any expression                                         
 that evals to a valid string..                                                                                 
 The current format is chosen to be windoze compatible.                                                         
 The earlier format was:                                                                                        
  -%Y-%m-%d:%H%M:%S"                                                                                            
  )                                                                                                              


(defcustom mkback-time-format-modtime                                                                          
  "-%Y%m%d-%H%M-%S--"                                                                                          
  "The string to use for time-format..  More generally, any expression                                         
 that evals to a valid string..                                                                                 
 We get the last-modified-time of the file and use it here. ")                                                  


(defcustom mkback-loudness 100                                                                                 
  "suggested: Anywhere from 0 to 100"                                                                          
  )                                                                                                              
(defcustom mkback-interactivity 100                                                                            
  "Suggested: Anywhere from -100  to 100..                                                                     
 if this number is too low, mkback will ask you for less and less                                               
 confirmations.                                                                                                 
 0 is the recommended value once you are familiar with mkback. "                                                
  )                                                                                                              

(defvar mkback-err nil "internal")                                                                             
(defcustom mkback-default-get-backup-dir 'mkback-get-backup-dir                                                
  "")                                                                                                          
(defcustom mkback-default-get-backup-path-name 'mkback-get-backup-path-name                                    
  "")                                                                                                          
(defcustom mkback-default-get-backup-file-name 'mkback-get-backup-file-name                                    
  "")                                                                                                          



(defmacro mkback-withit (expr &rest rest)                                                                      
  "Caution: var-capture by its very nature.."                                                                  
  `(let ((it ,expr))                                                                                           
     ,@rest))                                                                                                  

(defcustom mkback-max-depth 4                                                                                  
  "Is an integer.. this is the max number of ancestors to ascend to look                                       
 for an archive directory.                                                                                      
                                                                                                                
 A value of nil here means: no max number..  Nil is not currently                                               
 recommended as can potentially cause infinite looping if no backup/                                            
 exists in the entire ancestory.")                                                                              


(defcustom mkback-keep-time-p t                                                                                
  "When true, gives the destination file the same last-modified-time                                           
   as that of the original.")                                                                                   


;;;###autoload                                                                                                 
(defun mkback-get-backup-dir (dir &optional suffix depth)                                                      
  "An example of arguments is:                                                                                 
 \(mkback-get-backup-dir /home/aa/bb dd\).                                                                      
 Then, this function looks for a backup directory in /home/aa/bb.  If                                           
 it exists, then this function returns:  /home/aa/bb/backup/dd.                                                 
 Else this function calls                                                                                       
 \(mkback-get-backup-dir \"/home/aa\" \"dd/bb\" \"ff\" \).                                                      
                                                                                                                
 See what i mean? If no backup/ exists here, then a backup/ exists in a                                         
 parent directory.. but then, you want to create aa/ first in that                                              
 directory when creating backup, don't you?  So, this function returns                                          
 that...                                                                                                        
                                                                                                                
 Returns nil if can't find any.                                                                                 
 "                                                                                                              
  (unless depth (setq depth 0))                                                                                
  (if (and mkback-max-depth (> depth mkback-max-depth))                                                        
      nil                                                                                                      
    (progn                                                                                                     
      ;;(unless dir (setq dir default-directory))                                                              
      (unless suffix (setq suffix ""))                                                                         
      (mkback-message 25 "Considering dir= %S and suffix=%S" dir suffix)                                       
      (let* ((dir-unslashed (expand-file-name "" dir))                                                         
             (dir-backup (expand-file-name mkback-dir dir))                                                    
             (dir-backup-suf (expand-file-name suffix dir-backup)))                                            
        (if                                                                                                    
            (and (file-exists-p dir-backup)                                                                    
                 (file-directory-p dir-backup))                                                                
            dir-backup-suf                                                                                     
          (mkback-get-backup-dir                                                                               
           ;; parent dir                                                                                       
           (file-name-directory  dir-unslashed)                                                                
           ;; increase suffix                                                                                  
           (mkback-withit (file-name-nondirectory dir-unslashed)                                               
                          (if (equal suffix "")                                                                       
                              it                                                                                      
                            ;; commenting this out.. should NOT use /                                                 
                            ;;(concat it "/" suffix)                                                                  
                            (concat (file-name-as-directory it) suffix)                                               
                            ))                                                                                        
           (+ depth 1)))))))                                                                                   



(defun mkback-chase-links (file)                                                                               
  (case mkback-chase-links-method                                                                              
    ('dir                                                                                                      
     (let ((dir (or                                                                                            
                 (file-name-directory file)                                                                    
                 ;; else take the current directory... this comes in                                           
                 ;; handy when calling mkback-from-batch.                                                      
                 default-directory )))                                                                         
       ;; since we supply default-directory,                                                                   
       ;; this if is now mute...  but let's keep it.                                                           
       (if dir                                                                                                 
           (expand-file-name (file-name-nondirectory file)                                                     
                             (file-truename dir))                                                              
         file)))                                                                                               
    ('file (file-chase-links file))                                                                            
    ('all (file-truename file))                                                                                
    (t file)))                                                                                                 

;;;###autoload                                                                                                 
(defun mkback-get-backup-path-name (file &optional backup-dir                                                  
                                         )                                                                        
  "Looks around for a suitable backup/ directory nearby and returns a                                          
 suitable backup pathname.                                                                                      
                                                                                                                
 this is one heck of a powerful function..                                                                      
                                                                                                                
 SHOULD REALLY USE the function file-name-directory!                                                            
 "                                                                                                              
  (setq file (mkback-chase-links file))                                                                        
  
  (let                                                                                                         
      ((initdir                                                                                                
        (file-name-directory (expand-file-name file))))                                                        
    (unless backup-dir                                                                                         
      (setq backup-dir (funcall mkback-default-get-backup-dir                                                  
                                initdir)))                                                                             
    (if (not backup-dir)                                                                                       
        (if mkback-create-new-backup-dir-p                                                                     
            (setq backup-dir                                                                                   
                  (expand-file-name mkback-dir                                                                 
                                    initdir))                                                                  
          (error "No archi(v)e directory exists here or nearby. "))))                                          
  
  (funcall mkback-default-get-backup-file-name file backup-dir))                                               



(defun mkback-get-backup-file-name (file dir)                                                                  
  "File is the original file, dir is the destination directory.                                                
 This function will thus rename file with date appended, and then                                               
 append the same to the destination directory. "                                                                
  (let*                                                                                                        
      ((baseinit (file-name-sans-extension                                                                     
                  (file-name-nondirectory file)))                                                                  
       (extinit (file-name-extension file))                                                                    
       (base                                                                                                   
        (if (string= baseinit "")                                                                              
            (concat "." extinit) baseinit))                                                                    
       (ext2                                                                                                   
        (if (string= baseinit "")                                                                              
            nil extinit))                                                                                      
       (raw-name-file                                                                                          
        (concat                                                                                                
         base                                                                                                  
         (format-time-string                                                                                   
          (eval mkback-time-format-modtime)                                                                    
          (nth 5 (file-attributes file)))                                                                      
         (format-time-string (eval mkback-time-format))                                                        
         ))                                                                                                    
       (raw-name                                                                                               
        (expand-file-name                                                                                      
         raw-name-file                                                                                         
         dir))                                                                                                 
       (ext                                                                                                    
        (mkback-withit ext2                                                                                    
                       (if (null it) ""                                                                        
                         (concat "." it )))))                                                                  
    (while                                                                                                     
        (file-exists-p (concat raw-name ext))                                                                  
      (setq raw-name (concat raw-name "a")))                                                                   
    (concat raw-name ext)))                                                                                    


(defun mkback-message (points &rest args)                                                                      
  (when (> (+ points mkback-loudness) 50)                                                                      
    (apply #'message args)))                                                                                   



;;; 2002-05-03 T09:41:03-0400 (Friday)    D. Goel                                                              
(defun mkback-no-errors (file)                                                                                 
  (mkback-ignore-errors (mkback file)))                                                                        


(defvar mkback-after-backup-hook nil                                                                           
  "Each of the functions in this hook shall take two arguments: the                                            
 full name of the original file and the ful name of the backuped                                                
 file. ")                                                                                                       

(defcustom mkback-gzip-p nil                                                                                   
  "Whether to gzip the mkbacked files. More generally, any post-backup                                         
 action to perform on the backuped file. ")                                                                     

(defcustom mkback-gzip-expression                                                                              
  '(when                                                                                                       
       (> (nth 7 (file-attributes it)) 250)                                                                    
     (shell-command (format "gzip %s" it)))                                                                    
  "Use it for the filename here. The current expression works only on                                          
 gnulinux type systems.")                                                                                       

;;;###autoload                                                                                                 
(defun mkback (&optional file)                                                                                 
  "Backup file/files.                                                                                          
 With no argument, will prompt for file.  If file is a list of files instead of                                 
 one file, will loop over them.                                                                                 
                                                                                                                
 When file is a single file, Returns nil if backup fails, returns                                               
 non-nil otherwise.                                                                                             
                                                                                                                
 When file is a list of files, returns the list of such results.                                                
 "                                                                                                              
  (interactive "F")                                                                                            
  (unless file                                                                                                 
    (setq file (read-file-name "File: ")))                                                                     
  (unless file (error "No filaname supplied to mkback: nil"))                                                  
  (if (listp file)                                                                                             
      (mapcar #'mkback-no-errors file)                                                                         
    (progn                                                                                                     
      (unless (file-exists-p file)                                                                             
        (error "File does not exist: %S" file))                                                                
      (when (file-directory-p file)                                                                            
        (error "Currently, can archive only files, not directies: %S" file))                                   
      (mkback-withit                                                                                           
       (funcall mkback-default-get-backup-path-name file)                                                      
       (let* ((dir (file-name-directory it))                                                                   
              (dir-existsp (file-exists-p dir))                                                                
              (failed nil))                                                                                    
         (unless dir-existsp                                                                                   
           (if (mkback-y-or-n-p 50                                                                             
                                (format "Create directory %S" dir))                                                            
               (make-directory dir t)                                                                          
             (mkback-message 99 "Not creating directory!")))                                                   
         (setq dir-existsp (and (file-exists-p dir) (file-directory-p dir)))                                   
         (setq failed (not dir-existsp))                                                                       
         (unless failed                                                                                        
           (if                                                                                                 
               (mkback-y-or-n-p 0                                                                              
                                (format "Copy %S to %S" (file-name-nondirectory file)                                          
                                        it))                                                                                   
               (progn                                                                                          
                 (copy-file file it nil mkback-keep-time-p))                                                   
             (setq failed t)))                                                                                 
         (run-hook-with-args 'mkback-after-backup-hook file it)                                                
         (setq failed (not (file-exists-p it)))                                                                
         (when mkback-gzip-p                                                                                   
           (ignore-errors                                                                                      
             (eval                                                                                             
              mkback-gzip-expression)))                                                                        
         (if failed                                                                                            
             (mkback-message 99 "File not created: %s " it)                                                    
           (mkback-message 99 "Backup (now) exists:\n %s" it))                                                 
         (not failed))))))                                                                                     




;;;###autoload                                                                                                 
(defun mkback-install-for-eshell ()                                                                            
  (interactive)                                                                                                
  (defalias 'eshell/mkback 'mkback))                                                                           


(defmacro mkback-ignore-errors (&rest body)                                                                    
  "\(Programmer: This function should track my ignore-errors-my. \)                                            
                                                                                                                
 Like ignore-errors, but tells the error..                                                                      
 Improved for me by Kalle on 7/3/01:                                                                            
  * used backquote: something i was too lazy to convert my macro to..                                           
  * removed the progn: condition-case automatically has one..                                                   
  * made sure that the return is nil.. just as it is in ignore-errors. "                                        
  `(condition-case mkback-err (progn ,@body)                                                                   
     (error                                                                                                    
      (ding t)                                                                                                 
      (ding t)                                                                                                 
      (ding t)                                                                                                 
      (message "IGNORED ERROR: %s" (error-message-string mkback-err))                                          
      (sit-for 1)                                                                                              
      nil)))                                                                                                   




;;; 2002-05-03 T11:05:43-0400 (Friday)    D. Goel                                                              
(defun mkback-y-or-n-p (add &rest args)                                                                        
  (if (> (+ add mkback-interactivity) 50)                                                                      
      (apply 'y-or-n-p args)                                                                                   
    t))                                                                                                        


;;; 2002-05-03 T11:07:10-0400 (Friday)    D. Goel                                                              
;;;###autoload                                                                                                 
(defun mkback-buffer ()                                                                                        
  (interactive)                                                                                                
  (mkback-withit                                                                                               
   (buffer-file-name)                                                                                          
   (if it (mkback it)                                                                                          
     (mkback-message 0 "Buffer has no associated file: %S"                                                     
                     (buffer-name)))))                                                                          

;;; 2002-05-03 T11:10:32-0400 (Friday)    D. Goel                                                              
;;;###autoload                                                                                                 
(defun mkback-buffer-doit ()                                                                                   
  (interactive)                                                                                                
  (let ((mkback-interactivity -100))                                                                           
    (mkback-buffer)))                                                                                          


(defmacro mkback-from-batch (&rest files)                                                                      
  "The files get passed to emacs as symbols.. we need to simply format                                         
 them.."                                                                                                        
  `(mkback                                                                                                     
    (mkback-symbols-to-strings (quote ,files))))                                                               

(defun mkback-symbols-to-strings (files)                                                                       
  (cond                                                                                                        
   ((null files) nil)                                                                                          
   ((listp files) (mapcar 'mkback-symbols-to-strings files))                                                   
   (t (format "%s" files))))                                                                                   





(defmacro mkback-from-batch-doit (&rest files)                                                                 
  "The files get passed to emacs as symbols.. we need to simply format                                         
 them.."                                                                                                        
  `(let ((mkback-interactivity -100))                                                                          
     (mkback                                                                                                   
      (mkback-symbols-to-strings (quote ,files)))))                                                            


(defun mkback-symbols-to-strings (files)                                                                       
  (cond                                                                                                        
   ((null files) nil)                                                                                          
   ((listp files) (mapcar 'mkback-symbols-to-strings files))                                                   
   (t (format "%s" files))))                                                                                   


;; these 2 provided for historical compatibility for the next few versions..                                   
;; and THEY WILL BE REMOVED SOON..                                                                             
(defalias 'mkback-this-buffer 'mkback-buffer)                                                                  
(defalias 'mkback-this-buffer-doit 'mkback-buffer-doit)                                                        

(provide 'mkback)                                                                                              
(run-hooks 'mkback-after-load-hooks)                                                                           



;;; mkback.el ends here                                                                                        
