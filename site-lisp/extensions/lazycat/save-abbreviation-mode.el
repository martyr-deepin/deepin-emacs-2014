;;; save-abbreviation-mode.el - magically save and load aliases for emacs lisp functions 

;; Copyright (c) 2008 Tom Wright

;; Author: Tom Wright
;; Maintainer: Tom Wright
;; Keywords: convenience

;; This file is distributed under the MIT license: 
;; http://www.opensource.org/licenses/mit-license.php

;; This probably doesn't deserve to be called a mode. But having something that you can treat as
;; a black box and not hack together yourself repeatedly is probably fairly useful.

;;; Installation:
;; Place save-abbreviation-mode.el so that it is in your emacs path (see load-path variable "C-h v load-path")
;; Add 
;;  (require 'save-abbreviation-mode)
;;  (save-abbreviation-mode 1)
;; to your emacs file.
;;
;;; Caveats:
;; At the moment save-abbreviation-mode does not unload abbreviations if the mode is disabled. 
;; If you feel the need to this ask and I will probably add it in a couple of days or so.

;;; Code:

(defgroup save-abbreviation nil
  "Magically saving aliases to emacs lisps functions (abbreviations) to a file that is reloaded on startup.
This is not to be confused with the abbrev package.
There are other modes to provide automatic abbreviations for all emacs lisp functions."
  :tag "Save Abbreviation"
  :group 'save-abbreviaion
  :group 'convenience)

(defcustom save-abbreviation-file "~/.abbrev" "File where abbreviations should be saved." :group 'save-abbreviaion)

;;;###autoload
(defun save-abbreviation (command-string abbrev)
  "Add an abbreviation (an alias for a function) to the abbreviation list."
  (interactive "CCommand to make an abbreviation for:\nsAbbreviation:")
  (let ((abbrev-open
         (member (file-truename save-abbreviation-file) (mapcar 'buffer-file-name (buffer-list))))
        (abbrev-buffer (find-file-noselect save-abbreviation-file)))
    (assert (not (fboundp (intern abbrev))) "This abbreviation is
already bound")
    (save-excursion
      (set-buffer abbrev-buffer)
      ;; (end-of-buffer)
      (goto-char (point-max))
      (insert (format "(defalias '%s '%s)\n" abbrev command-string))
      (eval-buffer)
      (sort-lines nil (buffer-end -1) (buffer-end 1))
      (basic-save-buffer))
    (if (not abbrev-open)
        (kill-buffer abbrev-buffer))))

;;;###autoload
(define-minor-mode save-abbreviation-mode
  "Toggle save-abbreviation mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.
     
  Save-abbreviation-mode lets one interactively create alias 
  for emacs lisp functions - these are saved to disk and 
  restored the next time save-abbreviation mode is enabled.
  However these aliases will not be undefined when save 
  abbreviation mode is disabled - this will only happen if
  they are removed by hand or emacs is restarted"
  
  nil
  ;; The indicator for the mode line.
  nil
  ;; The minor mode bindings.
  nil
  :group 'save-abbreviation)

(defun save-abbreviation-load-abbrevs ()
  (message (format "Loading abbreviations file:%s" save-abbreviation-file))
  (if (file-exists-p save-abbreviation-file)
      (load-file save-abbreviation-file)))

(add-hook 'save-abbreviation-mode-hook  'save-abbreviation-load-abbrevs)

(save-abbreviation-load-abbrevs)

(provide 'save-abbreviation-mode)
;; save-abbreviation-mode.el ends here
