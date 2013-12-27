;; escreen にタブもどきをくっつけてみる．

(require 'cl)
(require 'escreen)

(defface escreen-tab-unselected-retrieving-face
  '((t :background "Gray70" :foreground "Gray50"
       :underline "Gray85" :box (:line-width -1 :style released-button)))
  "unselected")

(defface escreen-tab-selected-retrieving-face
  '((t :background "Gray85" :foreground "black"
       :underline "Gray85" :box (:line-width -1 :style released-button)))
  "selected")

(defface escreen-tab-background-face
  '((t :background "LightSteelBlue" :foreground "black"
       :underline "Gray85"))
  "background")

(defun escreen-tab-create-name (name limit)
  (cond
   ((string-match "\\*navi2ch" name) "[navi2ch]")
   ((string-match "Messenger" name) "[messenger]")
   (t
    (if (< (length name) limit)
        name
      (concat (truncate-string-to-width name (- limit 3)) "...")))))

(defun escreen-tab-list ()
  (interactive)
  (escreen-configuration-alist-sort-by-number)
  (escreen-save-current-screen-configuration)
  (let ((alist escreen-configuration-alist)
        (limit 13)
        (result nil))
    (while alist
      (setq screen-data (car alist))
      (setq alist (cdr alist))
      (setq screen-number (escreen-configuration-screen-number screen-data))
      (setq data-map (escreen-configuration-data-map screen-data))
      (setq result
            (cons
             (propertize
              (concat
               (format (if (= screen-number escreen-current-screen-number) "%d+ " "%d- ") screen-number)
               (escreen-tab-create-name
                (escreen-configuration-data-map-critical-buffer-name
                 (escreen-configuration-data-map-critical (car data-map)))
                limit))
              'face (if (= screen-number escreen-current-screen-number)
                        'escreen-tab-selected-retrieving-face
                      'escreen-tab-unselected-retrieving-face))
             (cons
              (propertize " " 'face 'escreen-tab-background-face)
              result))))
    (cdr (nreverse
          (cons
           (propertize (make-string (window-width) ?\ ) 'face 'escreen-tab-background-face)
           result)))))

(defadvice escreen-goto-screen-hook (before escreen-modified-hook activate)
  (escreen-tab-mode-line-update))
(add-hook 'escreen-goto-screen-hook (lambda () (escreen-tab-mode-line-update)))

(defun escreen-tab-click (e)
  (interactive "e")
  (let* ((title (car (nth 4 (cadr e))))
         (m (string-match "^\\([0-9]\\)[+-] " title))
         (s (substring title (match-beginning 1) (match-end 1)))
         (n (string-to-int s)))
    (escreen-goto-screen n)))

(global-set-key [header-line down-mouse-1] 'ignore)
(global-set-key [header-line down-mouse-2] 'ignore)
;;(global-set-key [header-line drag-mouse-1] 'ignore)
;;(global-set-key [header-line drag-mouse-2] 'ignore)
(global-set-key [header-line mouse-1] #'escreen-tab-click)
(global-set-key [header-line mouse-2] #'escreen-tab-click)

(defun escreen-tab-mode-line-update ()
  (interactive)
  (let* ((s (selected-window))
         (lst (window-list))
         (w nil))
    (mapcar (lambda (e)
              (when (eq (window-at 0 0) e) (setq w e))
              (select-window e)
              (setq header-line-format nil)
              (force-mode-line-update)) lst)
    (select-window w)
    (setq header-line-format (when (eq *escreen-tab-show* :on) (escreen-tab-list)))
    (force-mode-line-update)
    (select-window s)))

(defvar *escreen-tab-show* :on)
(defun escreen-tab-toggle (&optional on-off)
  (interactive)
  (setq *escreen-tab-show*
        (if (or (eq on-off :on) (eq on-off :off))
            on-off
          (if (eq *escreen-tab-show* :on) :off :on)))
  (escreen-tab-mode-line-update))

(defmacro escreen-tab-set-hooks (&rest functions)
  (cons 'progn
        (mapcar (lambda (f)
                  `(defadvice ,f (after escreen-modified-hook activate)
                     (when (null (string-match "Minibuf" (buffer-name (current-buffer))))
                       (escreen-tab-mode-line-update))))
                functions)))

(escreen-tab-set-hooks split-window delete-window find-file switch-to-buffer)

(provide 'escreen-tab)
