;; 
;;   With Oort Gnus or Gnus v5.8.8, v5.9, you can put nnshimbun.el from 
;;   the latest version of T-gnus 6.15 distribution into the lisp/
;;   directory and run "make install", then you can browse article
;;   by using nnshimbun.  In addition, in order to use emacs-w3m
;;   instead of eamcs-w3 to browse contents of text/html, put the
;;   followings in .gnus file.

;; From: greg@visiontech-dml.com
;; Newsgroups: gnu.emacs.help
;; Subject: Re: w3m-mode and images
;; Date: 19 Jul 2001 10:59:19 +0300
;; Message-ID: <2fasnftcpt4.fsf@broadcom.com>

(defvar gnus-w3m-minor-mode nil)

(make-variable-buffer-local 'gnus-w3m-minor-mode)
(add-to-list 'minor-mode-alist '(gnus-w3m-minor-mode " w3m"))
(add-to-list 'minor-mode-map-alist (cons 'gnus-w3m-minor-mode w3m-mode-map))

(defadvice mm-inline-text (around use-w3m-instead (handle) activate)
  (let ((type (mm-handle-media-subtype handle)))
    (if (not (equal type "html"))
	ad-do-it
      (let ((text (mm-get-part handle))
	    (b (point)))
	(save-excursion
	  (insert text)
	  (save-restriction
	    (narrow-to-region b (point))
	    (goto-char (point-min))
	    (w3m-region (point-min) (point-max))
	    (setq gnus-w3m-minor-mode t))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(setq gnus-w3m-minor-mode nil)
		(if (functionp 'remove-specifier)
		    (mapcar (lambda (prop)
			      (remove-specifier
			       (face-property 'default prop)
			       (current-buffer)))
			    '(background background-pixmap foreground)))
		(delete-region ,(point-min-marker)
			       ,(point-max-marker))))))))))

;; Date: Fri, 27 Jul 2001 12:51:12 +0900
;; Message-ID: <yosuwv4v3u8f.fsf@jpl.org>
;; From: Katsumi Yamaoka <yamaoka@jpl.org>
;; To: semi-gnus-ja@meadowy.org
;; Subject: [nnshimbun] toggle inline images

;; Browse image in multipart/related in koizumi mail-mag.

(eval-after-load "gnus-art"
  '(or (assoc "multipart/related" gnus-mime-multipart-functions)
       (setq gnus-mime-multipart-functions
	     (cons
	      (cons
	       "multipart/related"
	       (byte-compile
		(lambda (handle)
		  (gnus-mime-display-mixed (cdr handle)))))
	      gnus-mime-multipart-functions))))

