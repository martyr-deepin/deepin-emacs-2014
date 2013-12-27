;;; emms-lyrics-download.el --- Download lyrics automatically

;; Copyright (C) 2008 Austin

;; Author: Austin <austiny.cn@gmail.com>
;; Keywords: emms music lyrics

;;; Commentary:

;; This package enables you to download lyrics automatically.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;              (require 'emms-lyrics-download)
;;              (ad-activate 'emms-lyrics-find-lyric)

;; Then lyrics will be downloaded into the folder that contains your music files
;; when lyrics is enabled (either by 'M-x emms-lyrics-enable' or add (emms-lyrics 1) in your .emacs)
;; Note that lyrics downloading depends the ID3 tag of the mp3.
;; So correct ID3 tag may help you download the correct lyrics.
;; You may re-download the lyrics by 'M-x emms-lyrics-redownload-lyric'.

;;; Code:

(defun http-url-encode (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
         (mapcar (lambda (c)
                   (if (or (and (>= c ?a) (<= c ?z))
                           (and (>= c ?A) (<= c ?Z))
                           (and (>= c ?0) (<= c ?9)))
                       (string c)
                     (format "%%%02x" c)))
                 (encode-coding-string str content-type))))

(defun emms-lyrics-download-winampcn (file idx)
  "Download lyrics from winampcn.com according to what's playing."
  (let* ((itrack (emms-playlist-current-selected-track))
         (artist (emms-track-get itrack 'info-artist))
         (title (emms-track-get itrack 'info-title))
         (iartist (replace-regexp-in-string "[ ,]" "" artist))
         (ititle (replace-regexp-in-string "[ ,]" "" title))
         (uartist (http-url-encode iartist 'gb2312))
         (utitle (http-url-encode ititle 'gb2312))
         (lurl (format "http://www.winampcn.com/lyrictransfer/get.aspx?song=%s&artist=%s&lsong=%s&Datetime=20060901" utitle uartist utitle))
         (lfile (url-file-local-copy lurl))
         (llist (with-temp-buffer
                  (insert-file-contents lfile)
                  (let* ((urllist ())
                         (st 0)
                         (ed 0))
                    (while (setq st (re-search-forward "!\\[CDATA\\[" nil t))
                      (when (setq ed (re-search-forward "\\]\\]" nil t))
                        (let* ((url (buffer-substring st (- ed 2))))
                          (setq urllist (nconc urllist (list url))))))
                    urllist)))
         (lrcurl (nth (- idx 1) llist)))
    (if (not (eq nil lrcurl))
        (progn
          (let* ((lfile2 (url-file-local-copy lrcurl))
                 (st 0)
                 lyrics-string)
            (with-temp-buffer
              (insert-file-contents lfile2)
              (setq lyrics-string (buffer-string))
              (kill-region (point-min) (point-max))
              (insert (emms-lyrics-filter lyrics-string))
              (setq st (re-search-forward "</head>" nil t))
              (write-region st (point-max) file nil nil nil nil))
            (message (format "Lyrics: %s - %s downloaded." artist title))))
      (message (format "Lyrics: %s - %s not found." artist title)))))

(defun emms-lyrics-filter (lyrics-string)
  "Filter for emms lyrics context."
  (while (string-match "" lyrics-string)
    (setq lyrics-string (replace-match "" nil nil lyrics-string 0)))
  lyrics-string)

(defadvice emms-lyrics-find-lyric (before emms-lyrics-find-download-lyric (file))
  "Adding lyrics downloading to emms-lyrics-find-lyric"
  (let* ((track (emms-playlist-current-selected-track))
         (dir emms-lyrics-dir)          ;set lyrics download directory 
         )
    (when (eq 'file (emms-track-get track 'type))
      ;; If find two or more lyric files, only return the first one. Good
      ;; luck! :-)
      (unless (file-exists-p (concat dir file)) ; same directory?
        (if emms-lyrics-display-on-minibuffer
            (emms-lyrics-download-winampcn (concat dir file) 1))
        ))))

(defun emms-lyrics-redownload-lyric (&optional idx)
  "Interactively redownload lyrics for current playing.
A prefix can be taken to specify which lyrics to download if there is multiple. (idx starts from 1)"
  (interactive "P")
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-get track 'name))
         (lrc (funcall emms-lyrics-find-lyric-function
                       (emms-replace-regexp-in-string
                        (concat "\\." (file-name-extension name) "\\'")
                        ".lrc"
                        (file-name-nondirectory name)))))
    (or idx (setq idx 1))
    (if emms-lyrics-display-on-minibuffer
        (emms-lyrics-download-winampcn lrc idx))
    ))

(provide 'emms-lyrics-download)

;;; emms-lyrics-download.el ends here
