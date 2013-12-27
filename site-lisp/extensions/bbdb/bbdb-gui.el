;;; -*- Mode:Emacs-Lisp -*-
;;; This file contains font and menu hacks for BBDB.

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1992, 1993, 1994 Jamie Zawinski <jwz@netscape.com>.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This code is kind of kludgey, mostly because it needs to parse the contents
;;; of the *BBDB* buffer, since BBDB doesn't save the buffer-positions of the
;;; various fields when it fills in that buffer (doing that would be slow and
;;; cons a lot, so it doesn't seem to be worth it.)

(require 'bbdb)
(require 'bbdb-com)

;; compiler whinage. Some of this is legacy stuff that would probably
;; be better deleted.
(defvar scrollbar-height nil)

;; MIGRATE XXX
(eval-and-compile
  (if (fboundp 'set-specifier)
      (defalias 'bbdb-set-specifier 'set-specifier)
    (defalias 'bbdb-set-specifier 'ignore))
  (if (fboundp 'make-glyph)
      (defalias 'bbdb-make-glyph 'make-glyph)
    (defalias 'bbdb-make-glyph 'ignore))
  (if (fboundp 'set-glyph-face)
      (defalias 'bbdb-set-glyph-face 'set-glyph-face)
    (defalias 'bbdb-set-glyph-face 'ignore))
  (if (fboundp 'highlight-headers-x-face)
      (defalias 'bbdb-highlight-headers-x-face 'highlight-headers-x-face)
    (defalias 'bbdb-highlight-headers-x-face 'ignore))
  (if (fboundp 'highlight-headers-x-face-to-pixmap)
      (defalias 'bbdb-highlight-headers-x-face-to-pixmap
            'highlight-headers-x-face-to-pixmap)
    (defalias 'bbdb-highlight-headers-x-face-to-pixmap 'ignore)))


(if (featurep 'xemacs)
    (progn
      (define-key bbdb-mode-map 'button3 'bbdb-menu)
      (define-key bbdb-mode-map 'button2
        (lambda (e)
          (interactive "e")
          (mouse-set-point e)
          (bbdb-toggle-records-display-layout nil))))
  (define-key bbdb-mode-map [mouse-3] 'bbdb-menu)
  (define-key bbdb-mode-map [mouse-2]
    (lambda (e)
      (interactive "e")
      (mouse-set-point e)
      (bbdb-toggle-records-display-layout nil))))

(eval-and-compile
  (if (fboundp 'find-face)
      (defalias 'bbdb-find-face 'find-face)
    (if (fboundp 'internal-find-face) ;; GRR.
    ;; This should be facep in Emacs 21
        (defalias 'bbdb-find-face 'internal-find-face)
      (defalias 'bbdb-find-face 'ignore)))) ; noop - you probably don't HAVE faces.

(or (bbdb-find-face 'bbdb-name)
    (face-differs-from-default-p (make-face 'bbdb-name))
    (set-face-underline-p 'bbdb-name t))

(condition-case nil
    (or (bbdb-find-face 'bbdb-company)
        (face-differs-from-default-p (make-face 'bbdb-company))
        (make-face-italic 'bbdb-company)) ;; this can fail on emacs
  (error nil))

(or (bbdb-find-face 'bbdb-field-value)
    (make-face 'bbdb-field-value))

(or (bbdb-find-face 'bbdb-field-name)
    (face-differs-from-default-p (make-face 'bbdb-field-name))
    (copy-face 'bold 'bbdb-field-name))

;;; Extents vs. Overlays unhappiness
;;; FIXME: see if VM is around, and call its extents code instead;
;;; change bbdb-foo-extents below to vm-foo-extents, etc.
(eval-and-compile
  (if (fboundp 'make-extent)
      (defalias 'bbdb-make-extent 'make-extent)
    (defalias 'bbdb-make-extent 'make-overlay))

  (if (fboundp 'delete-extent)
      (defalias 'bbdb-delete-extent 'delete-extent)
    (defalias 'bbdb-delete-extent 'delete-overlay))

  (if (fboundp 'mapcar-extents)
      (defmacro bbdb-list-extents() `(mapcar-extents 'identity))
    (defun bbdb-list-extents()
      (let ((o (overlay-lists))) (nconc (car o) (cdr o)))))

  (if (fboundp 'mapcar-extents)
      (defmacro bbdb-extents-in (s e)
        (list 'mapcar-extents ''identity nil nil s e))
    (defmacro bbdb-extents-in (s e)
      (list 'overlays-in s e)))

  (if (fboundp 'set-extent-property)
      (defalias 'bbdb-set-extent-property 'set-extent-property)
    (defun bbdb-set-extent-property( e p v )
      (if (eq 'highlight p)
          (if v
              (overlay-put e 'mouse-face 'highlight)
            (overlay-put e 'mouse-face nil)))
      (overlay-put e p v)))

  (if (fboundp 'extent-property)
      (defalias 'bbdb-extent-property 'extent-property)
    (defalias 'bbdb-extent-property 'overlay-get))

  (if (fboundp 'extent-at)
      (defalias 'bbdb-extent-at 'extent-at)
    (defun bbdb-extent-at (pos buf tag) "NOT FULL XEMACS IMPLEMENTATION"
      (let ((o (overlays-at pos))
            minpri retval)
        (while (car o)
          (let ((x (car o)))
            (and (overlayp x)
                 (overlay-get x tag)
                 (if (or (null minpri) (> minpri (overlay-get x 'priority)))
                     (setq retval x
                           minpri (overlay-get x 'priority))))
            (setq o (cdr o))))
        retval)))

  (if (fboundp 'highlight-extent)
      (defalias 'bbdb-highlight-extent 'highlight-extent)
    (defalias 'bbdb-highlight-extent 'ignore)) ; XXX noop

  (if (fboundp 'extent-start-position)
      (defalias 'bbdb-extent-start-position 'extent-start-position)
    (defalias 'bbdb-extent-start-position 'overlay-start))

  (if (fboundp 'extent-end-position)
      (defalias 'bbdb-extent-end-position 'extent-end-position)
    (defalias 'bbdb-extent-end-position 'overlay-end))

  (if (fboundp 'extent-face)
      (defalias 'bbdb-extent-face 'extent-face)
    (defun bbdb-extent-face (extent)
      (overlay-get extent 'face)))

  (if (fboundp 'set-extent-face)
      (defalias 'bbdb-set-extent-face 'set-extent-face)
    (defun bbdb-set-extent-face (extent face) "set the face for an overlay"
      (overlay-put extent 'face face)))

  (if (fboundp 'set-extent-begin-glyph)
      (defalias 'bbdb-set-extent-begin-glyph 'set-extent-begin-glyph)
    (defalias 'bbdb-set-extent-begin-glyph 'ignore)) ; XXX noop

  (if (fboundp 'set-extent-end-glyph)
      (defalias 'bbdb-set-extent-end-glyph 'set-extent-end-glyph)
    (defalias 'bbdb-set-extent-end-glyph 'ignore))) ; XXX noop


(eval-when-compile (defvar scrollbar-height))
;;;###autoload
(defun bbdb-fontify-buffer (&optional records)
  (interactive)
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (if (featurep 'scrollbar)
        (bbdb-set-specifier scrollbar-height (cons (current-buffer) 0)))

    (let ((rest (or records bbdb-records))
          record face
          start end  s e
          multi-line-p
          property
          extent)

      (while rest
        (setq record (car (car rest))
              multi-line-p (string-match "multi-line"
                                        (symbol-name (nth 1 (car rest))))
              face (and multi-line-p (bbdb-record-getprop record 'face))
              start (marker-position (nth 2 (car rest)))
              end (1- (or (nth 2 (car (cdr rest))) (point-max))))

        (if (< start (point-min)) (setq start (point-min)))
        (if (> end (point-max)) (setq end (point-max)))

        (mapcar (function (lambda(o)
                            (if (and o
                                     (eq (bbdb-extent-property o 'data)
                                         'bbdb))
                                (bbdb-delete-extent o))))
                (bbdb-extents-in start end))

        (setq extent (bbdb-make-extent start end))
        (bbdb-set-extent-property extent 'highlight t)
        (bbdb-set-extent-property extent 'data 'bbdb)
        ;; note that on GNU Emacs, once you hit the main overlay, you
        ;; have to move off the record and back on again before it'll
        ;; notice that you're on a more specific overlay. This is
        ;; bogus, like most GNU Emacs GUI stuff.
        (bbdb-set-extent-property extent 'priority 3)
        (if face (bbdb-hack-x-face face extent))
        (goto-char start)
        (setq s start)
        (setq property (cadr (member 'bbdb-field (text-properties-at s))))
        (while (and s (< s end))
          (setq e (or (next-single-property-change (1+ s) 'bbdb-field)
                      (point-max)))
          (cond ((equal property '(name))
                 (setq extent (bbdb-make-extent s e))
                 (bbdb-set-extent-property extent 'priority 2)
                 (bbdb-set-extent-property extent 'data 'bbdb)
                 (bbdb-set-extent-face extent 'bbdb-name))
                ((equal property '(company))
                 (setq extent (bbdb-make-extent s e))
                 (bbdb-set-extent-property extent 'priority 2)
                 (bbdb-set-extent-property extent 'data 'bbdb)
                 (bbdb-set-extent-face extent 'bbdb-company))
                ((member 'field-name property)
                 (goto-char s)
                 (setq extent (bbdb-make-extent s e))
                 (bbdb-set-extent-property extent 'priority 2)
                 (bbdb-set-extent-property extent 'data 'bbdb)
                 (bbdb-set-extent-face extent 'bbdb-field-name))
                (t
                 (setq extent (bbdb-make-extent start e))
                 (bbdb-set-extent-property extent 'priority 2)
                 (bbdb-set-extent-property extent 'data 'bbdb)
                 (bbdb-set-extent-face extent 'bbdb-field-value)))
          (setq s e)
          (while (and s (null (setq property
                                    (cadr (member 'bbdb-field
                                                  (text-properties-at s))))))
            (setq s (next-single-property-change s 'bbdb-field))))

        (setq rest (cdr rest))
        (if (null (caar rest))
            (setq rest nil))))))

;;; share the xface cache data with VM if it's around
(defvar vm-xface-cache (make-vector 29 0))
(eval-when-compile (defvar highlight-headers-hack-x-face-p))

;; In Emacs 21, this could use the x-face support from Gnus.
(defun bbdb-hack-x-face (face extent)
  "Process a face property of a record and honour it.
Not done for GNU Emacs just yet, since it doesn't have image support
as of GNU Emacs 20.7"
  (if (not (or (and (fboundp 'highlight-headers-hack-x-face-p)
                    (symbol-value (intern                          ;; compiler
                              "highlight-headers-hack-x-face-p"))) ;; ick.
               (and (featurep 'xemacs)
                    (string-match "^21\\." emacs-version)))) ;; XXX
      () ;; nothing doing
    (setq face (bbdb-split face "\n"))
    (while face
      (cond

       ;; ripped pretty much verbatim from VM; X Faces for recent XEmacsen.
       ((string-match "^21\\." emacs-version) ;; XXX how far back can I go?
        (condition-case nil
            (let* ((h (concat "X-Face: " (car face))) ;; from vm-display-xface
                   (g (intern h vm-xface-cache)))
              (if (bbdb-find-face 'vm-xface) ;; use the same face as VM
                  nil
                (make-face 'vm-xface)
                (set-face-background 'vm-xface "white")
                (set-face-foreground 'vm-xface "black"))
              (if (boundp g)
                  (setq g (symbol-value g))
                (set g (bbdb-make-glyph
                        (list
                         (vector 'xface ':data h)))) ;; XXX use API
                (setq g (symbol-value g))
                (bbdb-set-glyph-face g 'vm-xface))
              (bbdb-set-extent-property extent 'vm-xface t)
              (bbdb-set-extent-begin-glyph extent g))
          (error nil))) ;; looks like you don't have xface support, d00d

       ;; requires lemacs 19.10 version of highlight-headers.el
       ((fboundp 'highlight-headers-x-face)                     ; the 19.10 way
        (bbdb-highlight-headers-x-face (car face) extent)
        (let ((b (bbdb-extent-property extent 'begin-glyph)))
          (cond (b ; I'd like this to be an end-glyph instead
                 (bbdb-set-extent-property extent 'begin-glyph nil)
                 (bbdb-set-extent-property extent 'end-glyph b)))))

       ((fboundp 'highlight-headers-x-face-to-pixmap)           ; the 19.13 way
        (save-excursion
          (set-buffer (get-buffer-create " *tmp*"))
          (buffer-disable-undo (current-buffer))
          (erase-buffer)
          (insert (car face))
          (bbdb-set-extent-begin-glyph extent nil)
          (bbdb-set-extent-end-glyph extent
                                (bbdb-highlight-headers-x-face-to-pixmap
                                 (point-min) (point-max)))
          (erase-buffer))))

      ;; more faces?
      (setq face (cdr face))
      (cond (face ; there are more, so clone the extent
             (setq extent (bbdb-make-extent
                           (bbdb-extent-start-position extent)
                           (bbdb-extent-end-position extent)))
             (bbdb-set-extent-property extent 'data 'bbdb))))))


(defcustom bbdb-user-menu-commands nil
  "User defined menu entries which should be appended to the BBDB menu.
This should be a list of menu entries.
When set to a fucntion the function gets called with two arguments the
RECORD and the FIELD and it should either return nil or a list of menu
entries."
  :group 'bbdb-database
  :type 'sexp)

(defun build-bbdb-finger-menu (record)
  (let ((addrs (bbdb-record-finger-host record)))
    (if (cdr addrs)
        (cons "Finger..."
              (nconc
               (mapcar (lambda (addr)
                         (vector addr (list 'bbdb-finger record addr)
                                 t))
                       addrs)
               (list "----"
                     (vector "Finger all addresses"
                             (list 'bbdb-finger record ''(4)) t))))
      (vector (concat "Finger " (car addrs))
              (list 'bbdb-finger record (car addrs)) t))))

(defun build-bbdb-sendmail-menu (record)
  (let ((addrs (bbdb-record-net record)))
    (if (cdr addrs)
        (cons "Send Mail..."
              (mapcar (lambda (addr)
                        (vector addr (list 'bbdb-send-mail-internal
                                           (bbdb-dwim-net-address record addr))
                                t))
                      addrs))
      (vector (concat "Send mail to " (car addrs))
              (list 'bbdb-send-mail-internal
                    (bbdb-dwim-net-address record (car addrs)))
              t))))


(defun build-bbdb-field-menu (record field)
  (let ((type (car field)))
    (nconc
     (list
      (concat "Commands for "
              (cond ((eq type 'property)
                     (concat "\""
                             (symbol-name (if (consp (car (cdr field)))
                                              (car (car (cdr field)))
                                            (car (cdr field))))
                             "\" field:"))
                    ((eq type 'name) "Name field:")
                    ((eq type 'company) "Company field:")
                    ((eq type 'net) "Network Addresses field:")
                    ((eq type 'aka) "Alternate Names field:")
                    (t
                     (concat "\"" (aref (nth 1 field) 0) "\" "
                             (capitalize (symbol-name type)) " field:"))))
      "-----"
      ["Edit Field" bbdb-edit-current-field t]
      )
     (if (memq type '(name company))
         nil
       (list ["Delete Field" bbdb-delete-current-field-or-record t]))
     (cond ((eq type 'phone)
            (list (vector (concat "Dial " (bbdb-phone-string (car (cdr field))))
                          (list 'bbdb-dial (list 'quote field) nil) t)))
           )
     )))


(defun build-bbdb-insert-field-menu (record)
  (cons "Insert New Field..."
        (mapcar
         (lambda (field)
           (let ((type (if (string= (car field) "AKA")
                           'aka
                         (intern (car field)))))
             (vector (car field)
                     (list 'bbdb-insert-new-field
                           record
                           (list 'quote type)
                           (list 'bbdb-prompt-for-new-field-value
                                 (list 'quote type)))
                     (not
                      (or (and (eq type 'net) (bbdb-record-net record))
                          (and (eq type 'aka) (bbdb-record-aka record))
                          (and (eq type 'notes) (bbdb-record-notes record))
                          (and (consp (bbdb-record-raw-notes record))
                               (assq type (bbdb-record-raw-notes record))))))))
         (append '(("phone") ("address") ("net") ("AKA") ("notes"))
                 (bbdb-propnames)))))


(defun build-bbdb-menu (record field)
  (delete
   nil
   (append
    '("bbdb-menu" "Global BBDB Commands" "-----")
    (list
     ["Save BBDB" bbdb-save-db t]
     ["Toggle All Records Display Layout"
      bbdb-toggle-all-records-display-layout t]
    ["Finger All Records" (bbdb-finger (mapcar 'car bbdb-records)) t]
    ["BBDB Manual" bbdb-info t]
    ["BBDB Quit" bbdb-bury-buffer t])
    (if record
        (list
         "-----"
         (concat "Commands for record \""
                 (bbdb-record-name record) "\":")
         "-----"
         (vector "Delete Record"
                 (list 'bbdb-delete-current-record record) t)
         ["Toggle Records Display Layout" bbdb-toggle-records-display-layout t]
         (if (and (not (eq 'full-multi-line
                           (nth 1 (assq record bbdb-records))))
                  (bbdb-display-layout-get-option 'multi-line 'omit))
             ["Fully Display Record" bbdb-display-record-completely t])
         ["Omit Record" bbdb-omit-record t]
         ["Refile (Merge) Record" bbdb-refile-record t]
         ))
    (if record
        (list (build-bbdb-finger-menu record)))
    (if (bbdb-record-net record)
        (list (build-bbdb-sendmail-menu record)))
    (if record
        (list (build-bbdb-insert-field-menu record)))
    (if field
        (cons "-----" (build-bbdb-field-menu record field)))
    (if bbdb-user-menu-commands
        (let ((menu (if (functionp bbdb-user-menu-commands)
                        (funcall bbdb-user-menu-commands record field)
                      bbdb-user-menu-commands)))
          (if menu
              (append ["-----"]
                      ["User Defined Commands"]
                      ["-----"]
                      menu)))))))

(eval-and-compile
  (if (fboundp 'popup-menu)
      (progn
        (fset 'bbdb-popup 'popup-menu)
        (fset 'bbdb-desc-to-menu 'identity))
    ;; This is really, REALLY ugly, but it saves me some coding and uses
    ;; the correct keymap API instead of carnal knowledge of keymap
    ;; structure.
    (defun bbdb-desc-to-menu(desc)
      (let ((map (make-sparse-keymap (car desc)))
            (desc (reverse (cdr desc))) ;; throw away header, reorient list
            (txtcount 0) elt elt-name)
        (while (setq elt (car desc))
          ;; fake a key binding name
          (setq elt-name (intern (format "fake%d" txtcount))
                txtcount (+ 1 txtcount))
          (cond
           ;; non-active entries in the menu
           ((stringp elt)
            (define-key map (vector elt-name) (list elt)))

           ;; active entries in the menu
           ((vectorp elt)
            (define-key map (vector elt-name) (cons (aref elt 0) (aref elt 1))))

           ;; submenus
           ((listp elt)
            (define-key map (vector elt-name)
              (cons (car elt) (bbdb-desc-to-menu elt))))
           )
          (setq desc (cdr desc)))
        map))
    ;; this does the actual popping up & parsing nonsense
    (defun bbdb-popup( desc &optional event )
      (let ((map (bbdb-desc-to-menu desc)) result)
        (setq result (x-popup-menu t map))
        (if result
            (let ((command (lookup-key map (vconcat result))))
              ;; Clear out echoing, which perhaps shows a prefix arg.
              (message "")
              (if command
                  (if (commandp command)
                      (command-execute command)
                    (funcall 'eval command)))))))))

;;;###autoload
(defun bbdb-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (bbdb-popup
   (save-window-excursion
     (save-excursion
       (let ((extent (or (bbdb-extent-at (point) (current-buffer) 'highlight)
                         (error "")))
             record field)
         (or (eq (bbdb-extent-property extent 'data) 'bbdb)
             (error "not a bbdb extent"))
         (bbdb-highlight-extent extent t)
         (setq record (bbdb-current-record)
               field  (get-text-property (point) 'bbdb-field))
         (build-bbdb-menu record field))))))

;; tell everyone else we're here.
(provide 'bbdb-gui)
