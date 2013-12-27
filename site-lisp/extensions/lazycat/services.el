;;; services.el --- Services database access functions.
;; Copyright 2000,2003 by Dave Pearson <davep@davep.org>
;; $Revision: 1.3 $

;; services.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; services.el provides a set of functions for accessing the services
;; details list.
;;
;; The latest services.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/#services.el>

;;; BUGS:
;;
;; o Large parts of this code look like large parts of the code you'll find
;;   in protocols.el, this is unfortunate and makes me cringe. However, I
;;   also wanted them to be totally independant of each other. Suggestions
;;   of how to sweetly remedy this situation are welcome.

;;; INSTALLATION:
;;
;; o Drop services.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add the following autoload statement to your ~/.emacs file:
;;
;;   (autoload 'services-lookup      "services" "Perform a service lookup" t)
;;   (autoload 'services-clear-cache "services" "Clear the service cache"  t)

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl))

;; Customisable variables.

(defvar services-file "/etc/services"
  "*Name of the services file.")

;; Non-customize variables.

(defvar services-cache nil
  "\"Cache\" of services.")

(defvar services-name-cache nil
  "\"Cache\" of service names.")

;; Main code:

(defsubst service-name (service)
  "Get the name of service SERVICE."
  (car service))

(defsubst service-port (service)
  "Get the port of service SERVICE."
  (cadr service))

(defsubst service-protocols (service)
  "Get the protocols of service SERVICE."
  (car (cddr service)))

(defsubst service-aliases (service)
  "Get the aliases for service SERVICE."
  (cadr (cddr service)))

(defun services-line-to-list (line)
  "Convert LINE from a string into a structured service list."
  (let* ((words (split-string line))
         (port (split-string (cadr words) "/")))
    (list
     (car words)
     (string-to-int (car port))
     (list (cadr port))
     (loop for s in (cddr words)
           while (not (= (aref s 0) ?#))
           collect s))))

(defun* services-read (&optional (file services-file))
  "Read the services list from FILE.

If FILE isn't supplied the value of `services-file' is used."
  (or services-cache
      (setq services-cache
            (when (file-readable-p file)
              (with-temp-buffer
                (insert-file-contents-literally file)
                (setf (point) (point-min))
                (let ((services (list)))
                  (loop for service in
                        (loop until (eobp)
                              do (setf (point) (line-beginning-position))
                              unless (or (looking-at "^[ \t]*#") (looking-at "^[ \t]*$"))
                              collect (services-line-to-list (buffer-substring (line-beginning-position) (line-end-position)))
                              do (forward-line))
                        do (let ((hit (assoc (service-name service) services)))
                             (if (and hit (= (service-port hit) (service-port service)))
                                 (setf (cdr hit) (list
                                                  (service-port hit)
                                                  (append (service-protocols hit) (service-protocols service))
                                                  (service-aliases hit)))
                               (push service services)))
                        finally return (reverse services))))))))
      
(defun* services-find-by-name (name &optional (protocol "tcp") (services (services-read)))
  "Find the service whose name is NAME."
  (loop for service in services
        when (and (string= (service-name service) name)
                  (member protocol (service-protocols service)))
        return service))

(defun* services-find-by-port (port &optional (protocol "tcp") (services (services-read)))
  "Find the service whose port is PORT."
  (loop for service in services
        when (and (= (service-port service) port)
                  (member protocol (service-protocols service)))
        return service))

(defun* services-find-by-alias (alias &optional (protocol "tcp") (services (services-read)))
  "Find a the service whose with an alias of ALIAS."
  (loop for service in services
        when (and (member alias (service-aliases service))
                  (member protocol (service-protocols service)))
        return service))

;;;###autoload
(defun services-lookup (search protocol)
  "Find a service and display its details."
  (interactive (list
                (completing-read "Service Search: "
                                 (or services-name-cache
                                     (setq services-name-cache
                                           (loop for service in (services-read)
                                                 collect (list (service-name service))
                                                 append (loop for alias in (service-aliases service)
                                                              collect (list alias)))))
                                 nil nil "" nil)
                (completing-read "Protocol: " '(("tcp") ("udp")) nil nil "tcp" nil)))
  (let* ((services (services-read))
         (service (or (when (string-match "^[0-9]+$" search)
                        (services-find-by-port (string-to-int search) protocol services))
                      (services-find-by-name search protocol services)
                      (services-find-by-name (downcase search) protocol services)
                      (services-find-by-name (upcase search) protocol services)
                      (services-find-by-alias search protocol services)
                      (services-find-by-alias (downcase search) protocol services)
                      (services-find-by-alias (upcase search) protocol services))))
    (if service
        (let ((aliases (service-aliases service))
              (protocols (service-protocols service)))
          (message "Service: %s  Port: %d  %s%s"
                   (service-name service)
                   (service-port service)
                   (if aliases
                       (format "Aliases: %s"
                               (with-output-to-string
                                   (loop for alias in (service-aliases service)
                                         do (princ alias) (princ " "))))
                     "")
                   (if protocols
                       (format "%sProtocols: %s"
                               (if aliases " " "")
                               (with-output-to-string
                                   (loop for protocol in protocols
                                         do (princ protocol) (princ " "))))
                     "")))
      (error "No service matching \"%s\" using protocol %s" search protocol))))

;;;###autoload
(defun services-clear-cache ()
  "Clear the services \"cache\"."
  (interactive)
  (setq services-cache      nil
        services-name-cache nil))

(provide 'services)

;;; services.el ends here.
