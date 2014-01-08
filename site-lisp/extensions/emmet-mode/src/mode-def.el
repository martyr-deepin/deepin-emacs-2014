;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emmet minor mode

(defgroup emmet nil
  "Customization group for emmet-mode."
  :group 'convenience)

(defun emmet-expr-on-line ()
  "Extract a emmet expression and the corresponding bounds
   for the current line."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties start end))
         (expr (emmet-regex "\\([ \t]*\\)\\([^\n]+\\)" line 2)))
    (if (first expr)
        (list (first expr) start end))))

(defcustom emmet-indentation 4
  "Number of spaces used for indentation."
  :type '(number :tag "Spaces")
  :group 'emmet)

(defun emmet-prettify (markup indent)
  (let ((first-col (format (format "%%%ds" indent) ""))
        (tab       (format (format "%%%ds" emmet-indentation) "")))
    (concat first-col
            (replace-regexp-in-string "\n" (concat "\n" first-col)
                                      (replace-regexp-in-string "    " tab markup)))))

(defun emmet-transform (input)
  (if (memq major-mode '(css-mode scss-mode sass-mode))
      (emmet-css-transform input)
    (emmet-html-transform input)))

;;;###autoload
(defun emmet-expand-line (arg)
  "Replace the current line's emmet expression with the corresponding expansion.
If prefix ARG is given or region is visible call `emmet-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `emmet-mode'."
  (interactive "P")
  (let* ((here (point))
         (preview (if emmet-preview-default (not arg) arg))
         (beg (if preview
                  (progn
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (point))
                (when mark-active (region-beginning))))
         (end (if preview
                  (progn
                    (end-of-line)
                    (skip-chars-backward " \t")
                    (point))
                (when mark-active (region-end)))))
    (if beg
        (progn
          (goto-char here)
          (emmet-preview beg end))
      (let ((expr (emmet-expr-on-line)))
        (if expr
            (let ((markup (emmet-transform (first expr))))
              (when markup
                (let ((pretty (emmet-prettify markup (current-indentation))))
                  (when pretty
                    (delete-region (second expr) (third expr))
                    (emmet-insert-and-flash pretty)
                    (when (and emmet-move-cursor-after-expanding (emmet-html-text-p markup))
                      (let ((p (point)))
                        (goto-char
                         (+ (- p (length pretty))
                            (emmet-html-next-insert-point pretty))))))))))))))

(defvar emmet-mode-keymap nil
  "Keymap for emmet minor mode.")

(if emmet-mode-keymap
    nil
  (progn
    (setq emmet-mode-keymap (make-sparse-keymap))
    (define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line)
    (define-key emmet-mode-keymap (kbd "<C-return>") 'emmet-expand-line)))

;;;###autoload
(define-minor-mode emmet-mode
  "Minor mode for writing HTML and CSS markup.
With emmet for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{emmet-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/Emmet'.

See also `emmet-expand-line'."
  :lighter " Emmet"
  :keymap emmet-mode-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emmet yasnippet integration

(defun emmet-transform-yas (input)
  (let* ((leaf-count 0)
         (emmet-leaf-function
          (lambda ()
            (format "$%d" (incf leaf-count)))))
    (emmet-transform input)))

;;;###autoload
(defun emmet-expand-yas ()
  (interactive)
  (let ((expr (emmet-expr-on-line)))
    (if expr
        (let* ((markup (emmet-transform-yas (first expr)))
               (filled (replace-regexp-in-string "><" ">\n<" markup)))
          (delete-region (second expr) (third expr))
          (insert filled)
          (indent-region (second expr) (point))
          (yas/expand-snippet
           (buffer-substring (second expr) (point))
           (second expr) (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Real-time preview
;;

;;;;;;;;;;
;; Lennart's version

(defvar emmet-preview-input nil)
(make-local-variable 'emmet-preview-input)
(defvar emmet-preview-output nil)
(make-local-variable 'emmet-preview-output)
(defvar emmet-old-show-paren nil)
(make-local-variable 'emmet-old-show-paren)

(defface emmet-preview-input
  '((default :box t :inherit secondary-selection))
  "Face for preview input field."
  :group 'emmet)

(defface emmet-preview-output
  '((default :inherit highlight))
  "Face for preview output field."
  :group 'emmet)

(defvar emmet-preview-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emmet-preview-accept)
    (define-key map (kbd "<return>") 'emmet-preview-accept)
    (define-key map [(control ?g)] 'emmet-preview-abort)
    map))

(defun emmet-html-text-p (markup)
  (string-match "^[\s|\t|\n|\r]*<.*$" markup))

(defun emmet-preview-accept ()
  (interactive)
  (let ((ovli emmet-preview-input))
    (if (not (and (overlayp ovli)
                  (bufferp (overlay-buffer ovli))))
        (message "Preview is not active")
      (let* ((indent (current-indentation))
             (markup (emmet-preview-transformed indent)))
        (when markup
          (delete-region (line-beginning-position) (overlay-end ovli))
          (emmet-insert-and-flash markup)
          (when (and emmet-move-cursor-after-expanding (emmet-html-text-p markup))
            (let ((p (point)))
              (goto-char
               (+ (- p (length markup))
                  (emmet-html-next-insert-point markup)))))))))
  (emmet-preview-abort))

(defun emmet-html-next-insert-point (str)
  (let ((intag t)    (instring nil)
        (last-c nil) (c nil)
        (rti 0))
    (loop for i to (1- (length str)) do
          (setq last-c c)
          (setq c (elt str i))
          (case c
            (?\" (if (not (= last-c ?\\))
                     (progn (setq instring (not instring))
                            (when (and emmet-move-cursor-between-quotes
                                       (not instring)
                                       (= last-c ?\"))
                              (return i)))))
            (?>  (if (not instring)
                     (if intag
                         (if (= last-c ?/) (return (1+ i))
                           (progn (setq intag nil)
                                  (setq rti (1+ i))))
                       (return i)))) ;; error?
            (?<  (if (and (not instring) (not intag))
                     (setq intag t)))
            (?/  (if (and intag
                          (not instring)
                          (= last-c ?<))
                     (return rti)))
            (t
             (if (memq c '(?\t ?\n ?\r ?\s))
                 (progn (setq c last-c))
               (if (and (not intag) (not instring))
                   (return rti))))))))

(defvar emmet-flash-ovl nil)
(make-variable-buffer-local 'emmet-flash-ovl)

(defun emmet-remove-flash-ovl (buf)
  (with-current-buffer buf
    (when (overlayp emmet-flash-ovl)
      (delete-overlay emmet-flash-ovl))
    (setq emmet-flash-ovl nil)))

(defcustom emmet-preview-default t
  "If non-nil then preview is the default action.
This determines how `emmet-expand-line' works by default."
  :type 'boolean
  :group 'emmet)

(defcustom emmet-insert-flash-time 0.5
  "Time to flash insertion.
Set this to a negative number if you do not want flashing the
expansion after insertion."
  :type '(number :tag "Seconds")
  :group 'emmet)

(defcustom emmet-move-cursor-after-expanding t
  "If non-nil the the cursor position is
moved to before the first closing tag when the exp was expanded."
  :type 'boolean
  :group 'emmet)

(defcustom emmet-move-cursor-between-quotes nil
  "If emmet-move-cursor-after-expands is non-nil and this is non-nil then
cursor position will be moved to after the first quote."
  :type 'boolean
  :group 'emmet)

(defun emmet-insert-and-flash (markup)
  (emmet-remove-flash-ovl (current-buffer))
  (let ((here (point)))
    (insert markup)
    (setq emmet-flash-ovl (make-overlay here (point)))
    (overlay-put emmet-flash-ovl 'face 'emmet-preview-output)
    (when (< 0 emmet-insert-flash-time)
      (run-with-idle-timer emmet-insert-flash-time
                           nil 'emmet-remove-flash-ovl (current-buffer)))))

;;;###autoload
(defun emmet-preview (beg end)
  "Expand emmet between BEG and END interactively.
This will show a preview of the expanded emmet code and you can
accept it or skip it."
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (emmet-preview-abort)
  (if (not beg)
      (message "Region not active")
    (setq emmet-old-show-paren show-paren-mode)
    (show-paren-mode -1)
    (let ((here (point)))
      (goto-char beg)
      (forward-line 1)
      (unless (= 0 (current-column))
        (insert "\n"))
      (let* ((opos (point))
             (ovli (make-overlay beg end nil nil t))
             (ovlo (make-overlay opos opos))
             (info (propertize " Emmet preview. Choose with RET. Cancel by stepping out. \n"
                               'face 'tooltip)))
        (overlay-put ovli 'face 'emmet-preview-input)
        (overlay-put ovli 'keymap emmet-preview-keymap)
        (overlay-put ovlo 'face 'emmet-preview-output)
        (overlay-put ovlo 'before-string info)
        (setq emmet-preview-input  ovli)
        (setq emmet-preview-output ovlo)
        (add-hook 'before-change-functions 'emmet-preview-before-change t t)
        (goto-char here)
        (add-hook 'post-command-hook 'emmet-preview-post-command t t)))))

(defvar emmet-preview-pending-abort nil)
(make-variable-buffer-local 'emmet-preview-pending-abort)

(defun emmet-preview-before-change (beg end)
  (when
      (or (> beg (overlay-end emmet-preview-input))
          (< beg (overlay-start emmet-preview-input))
          (> end (overlay-end emmet-preview-input))
          (< end (overlay-start emmet-preview-input)))
    (setq emmet-preview-pending-abort t)))

(defun emmet-preview-abort ()
  "Abort emmet code preview."
  (interactive)
  (setq emmet-preview-pending-abort nil)
  (remove-hook 'before-change-functions 'emmet-preview-before-change t)
  (when (overlayp emmet-preview-input)
    (delete-overlay emmet-preview-input))
  (setq emmet-preview-input nil)
  (when (overlayp emmet-preview-output)
    (delete-overlay emmet-preview-output))
  (setq emmet-preview-output nil)
  (remove-hook 'post-command-hook 'emmet-preview-post-command t)
  (when emmet-old-show-paren (show-paren-mode 1)))

(defun emmet-preview-post-command ()
  (condition-case err
      (emmet-preview-post-command-1)
    (error (message "emmet-preview-post: %s" err))))

(defun emmet-preview-post-command-1 ()
  (if (and (not emmet-preview-pending-abort)
           (<= (point) (overlay-end emmet-preview-input))
           (>= (point) (overlay-start emmet-preview-input)))
      (emmet-update-preview (current-indentation))
    (emmet-preview-abort)))

(defun emmet-preview-transformed (indent)
  (let* ((string (buffer-substring-no-properties
		  (overlay-start emmet-preview-input)
		  (overlay-end emmet-preview-input))))
    (let ((output (emmet-transform string)))
      (when output
        (emmet-prettify output indent)))))

(defun emmet-update-preview (indent)
  (let* ((pretty (emmet-preview-transformed indent))
         (show (when pretty
                 (propertize pretty 'face 'highlight))))
    (when show
      (overlay-put emmet-preview-output 'after-string
                   (concat show "\n")))))

(provide 'emmet-mode)

;;; emmet-mode.el ends here
