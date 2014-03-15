;; adapted from https://raw.github.com/keturn/lintnode/master/flymake-jslint.el
;;
;; Commentary:
;;
;; Installation:
;;
;; Put this in your load-path, then add the following to your .emacs.
;;
;;     (require 'flymake-coffeelint)
;;     (add-hook 'coffee-mode-hook
;;         (lambda () (coffeelintnode-hook)))
;;
;; Configuration
;;
;; Do M-x customize-group flymake-coffeelint to customize paths, port and autostart.
;;
;; By default, all the coffeelint variables are on, however you can turn them off by adding
;; them to the variable coffeelintnode-coffeelint-excludes -
;;
;; Usage:
;;
;; To start lintnode, either
;; * run M-x coffeelintnode-start before invoking flymake.
;; * set the customize variable coffeelintnode-autostart to t
;;


;; Code:
(require 'flymake)

(defcustom coffeelintnode-node-program "node"
  "The program name to invoke node.js."
  :type 'string
  :group 'flymake-coffeelint)

(defcustom coffeelintnode-location "~/emacs/coffeelintnode"
  "The directory coffeelintnode's app.js may be found in."
  :type 'string
  :group 'flymake-coffeelint)

(defcustom coffeelintnode-port 3004
  "The port the coffeelintnode server runs on."
  :type 'integer
  :group 'flymake-coffeelint)

(defcustom coffeelintnode-autostart t
  "Whether to start coffeelintnode automatically when we've called coffeelintnode-hook"
  :type 'boolean
  :group 'flymake-coffeelint)

(defvar coffeelintnode-coffeelintrc "~/.coffeelintrc"
  "The path to .coffeelintrc")

(defun coffeelintnode-start ()
  "Start the coffeelintnode server.
Uses `coffeelintnode-node-program' and `coffeelintnode-location'."
  (interactive)
  (message "Starting coffeelintnode")
  (let
    (coffeelintnode-location (expand-file-name (concat coffeelintnode-location "/app.js")))
  )
  (start-process "coffeelintnode-server" "*coffeelintnode*"
                 coffeelintnode-node-program
                 coffeelintnode-location
                 "--port" (number-to-string coffeelintnode-port)
                 "--coffeelintrc" coffeelintnode-coffeelintrc)
  (sit-for 1)
  ;; (let ((counter 5))
  ;;   (while (and (not (get-buffer-process "*coffeelintnode*")) (> index 0))
  ;;  (sit-for 1)
  ;;  (setq index (1- counter))))
  )

(defun coffeelintnode-stop ()
  "stop the coffeelintnode server process"
  (interactive)
  (if (get-process "coffeelintnode-server")
      (kill-process "coffeelintnode-server")))

(defun coffeelintnode-restart()
  "Restart the coffeelintnode server - typically we've fiddled with the configuration"
  (interactive)
  (coffeelintnode-stop)
  (sit-for 1)
  (coffeelintnode-start))

(defun coffeelintnode-hook ()
  "When we open a file in coffeescript mode, we should check to see if there is a
   coffeelint process running. If there isn't we check to see if the user has set
   coffeelintnode-autostart to t, then start the process and flymake-mode."
  (let ((proc (get-buffer-process "*coffeelintnode*")))
    (if (not proc)
        (when coffeelintnode-autostart
          (coffeelintnode-start))
      (flymake-mode 1))))

(defun flymake-coffeelint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         (coffeelint-url (format "http://127.0.0.1:%d/coffeelint" coffeelintnode-port)))
    (list "curl" (list "--form" (format "source=<%s" local-file)
                       "--form" (format "filename=%s" local-file)
                       ;; FIXME: For some reason squid hates this curl invocation.
                       ;; "--proxy" ""
                       coffeelint-url))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.coffee$"
              flymake-coffeelint-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^Lint at line \\([[:digit:]]+\\): \\(.+\\)$"
              nil 1 nil 2)
            flymake-err-line-patterns))

(provide 'flymake-coffeelint)
