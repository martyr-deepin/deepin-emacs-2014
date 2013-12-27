;;; sample configuration for mldonkey.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; configuration of the basic module
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mldonkey)

;; not a mldonkey-mode feature but looks quite nice
(require 'highline)
(add-hook 'mldonkey-mode-hook 'highline-local-mode)

;; hostname and port
(setq mldonkey-host "localhost")
(setq mldonkey-port 4000) ; use the port of the telnet interface here

;; a nice picture for the motd
(setq mldonkey-image "~/.elisp/mldonkey.jpg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; configuration of the mldonkey-console module
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is the default
(setq mldonkey-console-use-color t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; configuration for the mldonkey-auth module
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; username and password
(setq mldonkey-user "admin")
(setq mldonkey-passwd "admin")

;; automatically authenticate after connecting to the core
(add-hook 'mldonkey-motd-hook 'mldonkey-auth)

;; automatically authenticate at the console
(add-hook 'mldonkey-console-hook 'mldonkey-console-auth)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; configuration for the mldonkey-vd module
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; with setting the following variables you can decide what to show.
;; these are the default values that should nicely fit in a 80 columns
;; terminal.  you can as well customize them.
(setq mldonkey-show-network nil)
(setq mldonkey-show-number t)
(setq mldonkey-show-filename t)
(setq mldonkey-show-percent t)
(setq mldonkey-show-downloaded nil)
(setq mldonkey-show-size nil)
(setq mldonkey-show-avail nil)
(setq mldonkey-show-days t)
(setq mldonkey-show-last-seen t)
(setq mldonkey-show-active-sources nil)
(setq mldonkey-show-total-sources nil)
(setq mldonkey-show-rate t)
(setq mldonkey-show-priority nil)
(setq mldonkey-show-finished-network nil)
(setq mldonkey-show-finished-number t)
(setq mldonkey-show-finished-filename t)
(setq mldonkey-show-finished-size t)
(setq mldonkey-show-finished-md4 nil)

;; this will convert "%20" to spaces in filenames
(add-to-list 'mldonkey-vd-filename-filters 'mldonkey-vd-filename-remove-p20)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; configuration of the mldonkey-vd-sort module
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set the functions how to sort the list of downloads.  See
;; M-x apropos RET mldonkey-vd-sort-dl- RET and
;; M-x apropos RET mldonkey-vd-sort-fin- RET
;; for a full list of all available functions.

;; the default is to sort by the number of the download.
(setq mldonkey-vd-sort-functions
      '((not mldonkey-vd-sort-dl-state)
        (not mldonkey-vd-sort-dl-percent)))

(setq mldonkey-vd-sort-fin-functions
      '(mldonkey-vd-sort-dl-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; configuration for the mldonkey-commands module
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically update the list of downloads after some commands

(add-hook 'mldonkey-pause-hook 'mldonkey-vd)
(add-hook 'mldonkey-resume-hook 'mldonkey-vd)
(add-hook 'mldonkey-commit-hook 'mldonkey-vd)
(add-hook 'mldonkey-recover-temp-hook 'mldonkey-vd)
