(defvar anything-c-imenu-cache-types nil)
(make-variable-buffer-local 'anything-c-imenu-cache-types)
(defvar anything-c-source-imenu-types
  '((name . "Imenu Types")
    (init . (lambda ()
              (require 'imenu)
              (setq anything-c-imenu-current-buffer
                    (current-buffer))
              (condition-case nil
                  (setq anything-c-imenu-cache-types
                        (assoc "Types" (imenu--make-index-alist)))
                (error nil))))
    (candidates . (lambda ()
                    (condition-case nil
                        (with-current-buffer anything-c-imenu-current-buffer
                          (mapcar (lambda (x)
                                    ;; Don't append cdr to history
                                    ;; it will corrupt file
                                    (car x))
                                  (remove-if-not (lambda (x)
                                                   (when (listp x)
                                                     (markerp (cdr x))))
                                                 anything-c-imenu-cache-types)))
                      (error nil))))
    (volatile)
    (action
     ("Imenu Types" . (lambda (item)
                        (anything-c-imenu item anything-c-imenu-cache-types))))))

(defvar anything-c-imenu-cache-var nil)
(make-variable-buffer-local 'anything-c-imenu-cache-var)
(defvar anything-c-source-imenu-variables
  '((name . "Imenu Variables")
    (init . (lambda ()
              (require 'imenu)
              (setq anything-c-imenu-current-buffer
                    (current-buffer))
              (condition-case nil
                  (setq anything-c-imenu-cache-var
                        (assoc "Variables" (imenu--make-index-alist)))
                (error nil))))
    (candidates . (lambda ()
                    (condition-case nil
                        (with-current-buffer anything-c-imenu-current-buffer
                          (mapcar (lambda (x)
                                    ;; Don't append cdr to history
                                    ;; it will corrupt file
                                    (car x))
                                  (remove-if-not (lambda (x)
                                                   (when (listp x)
                                                     (markerp (cdr x))))
                                                 anything-c-imenu-cache-var)))
                      (error nil))))
    (volatile)
    (action
     ("Imenu Variables" . (lambda (item)
                            (anything-c-imenu item anything-c-imenu-cache-var)))
     ("Describe variable" . (lambda (item)
                              (describe-variable (intern item))))
     ("Add variable to kill ring" . kill-new))))

(defvar anything-c-imenu-cache-func nil)
(make-variable-buffer-local 'anything-c-imenu-cache-func)
(defvar anything-c-source-imenu-functions
  '((name . "Imenu Functions")
    (init . (lambda ()
              (require 'imenu)
              (setq anything-c-imenu-current-buffer
                    (current-buffer))
              (condition-case nil
                  (setq anything-c-imenu-cache-func
                        (cddr (imenu--make-index-alist)))
                (error nil))))

    (candidates . (lambda ()
                    (condition-case nil
                        (with-current-buffer anything-c-imenu-current-buffer
                          (mapcar (lambda (x)
                                    ;; Don't append cdr to history
                                    ;; it will corrupt file
                                    (car x))
                                  (remove-if-not (lambda (x)
                                                   (when (listp x)
                                                     (markerp (cdr x))))
                                                 anything-c-imenu-cache-func)))
                      (error nil))))
    (volatile)
    (action
     ("Imenu Functions" . (lambda (item)
                            (anything-c-imenu item anything-c-imenu-cache-func)))
     ("Describe function" . (lambda (item)
                              (describe-function (intern item))))
     ("Add function to kill ring" . kill-new))
    (candidate-transformer . (lambda (candidates)
                               (anything-c-compose
                                (list candidates)
                                '(anything-c-mark-interactive-functions))))))

(defun anything-c-imenu (item cache)
  (imenu
   (assoc item cache))
  (highlight-regexp item)
  (sit-for 1)
  (unhighlight-regexp item))

(provide 'anything-c-imenu)
