;;; sb-sankei.el --- shimbun backend for the MSN Sankei News -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003-2011, 2013 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-sankei (shimbun-japanese-newspaper
				   shimbun-multi shimbun-rss) ())

(defvar shimbun-sankei-url "http://sankei.jp.msn.com/")

(defvar shimbun-sankei-top-level-domain "sankei.jp.msn.com")

(defvar shimbun-sankei-server-name "産経新聞")

(defvar shimbun-sankei-group-table
  '(("points" "注目ニュース"
     "http://sankei.jp.msn.com/rss/news/points.xml")
    ("flash" "速報"
     "http://sankei.jp.msn.com/rss/news/flash.xml")
    ("affairs" "事件"
     "http://sankei.jp.msn.com/rss/news/affairs.xml")
    ("politics" "政治"
     "http://sankei.jp.msn.com/rss/news/politics.xml")
    ("economy" "経済・IT"
     "http://sankei.jp.msn.com/rss/news/economy.xml")
    ("world" "国際"
     "http://sankei.jp.msn.com/rss/news/world.xml")
    ("sports" "スポーツ"
     "http://sankei.jp.msn.com/rss/news/sports.xml")
    ("entertainments" "エンタメ"
     "http://sankei.jp.msn.com/rss/news/entertainments.xml")
    ("life" "生活"
     "http://sankei.jp.msn.com/rss/news/life.xml")
    ("science" "科学"
     "http://sankei.jp.msn.com/rss/news/science.xml")
    ("region" "地方"
     "http://sankei.jp.msn.com/rss/news/region.xml")
    ;; 産経ニュース west
    ("west.flash" "最新ニュース"
     "http://sankei.jp.msn.com/rss/news/west_flash.xml")
    ("west.points" "注目ニュース"
     "http://sankei.jp.msn.com/rss/news/west_points.xml")
    ("west.affairs" "できごと"
     "http://sankei.jp.msn.com/rss/news/west_affairs.xml")
    ("west.sports" "スポーツ"
     "http://sankei.jp.msn.com/rss/news/west_sports.xml")
    ("west.life" "ライフ"
     "http://sankei.jp.msn.com/rss/news/west_life.xml")
    ("west.economy" "経済"
     "http://sankei.jp.msn.com/rss/news/west_economy.xml")
    ;; Non-RSS groups.
    ("column.sankeisho" "産経抄"
     "http://sankei.jp.msn.com/column/topics/column-14576-t1.htm")
    ("column.shucho" "主張"
     "http://sankei.jp.msn.com/column/topics/column-14593-t1.htm")
    ("column.seiron" "正論"
     "http://sankei.jp.msn.com/column/topics/column-14594-t1.htm")))

(defvar shimbun-sankei-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAGFBMVEX///8An/8Vb38CnwB
 Vv1X/vwD/fwD/PwA35I7FAAAAAXRSTlMAQObYZgAAAFpJREFUCNdjYEAF5TC6vICBUYCBgR3MEBQ
 ACoAZguwwhlJaWnoBA4OgkFFZWhqQwShspJaWFgDUK2yslpYKYjAbK4WGghgsLk6hoWBzXVzAAiA
 hVgiDwQHZfgCXhRLQU+g42QAAAABJRU5ErkJggg==")))

(defvar shimbun-sankei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-sankei))
  (mapcar 'car shimbun-sankei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-sankei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-sankei))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(defvar shimbun-sankei-retry-fetching 1)

(luna-define-method shimbun-headers :around ((shimbun shimbun-sankei)
					     &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      ;; Use the function defined in sb-rss.el.
      (luna-call-next-method)
    ;; Use the default function defined in shimbun.el.
    (funcall (intern "shimbun-headers"
		     (luna-class-obarray (luna-find-class 'shimbun)))
	     shimbun range)))

(luna-define-method shimbun-server-name :around ((shimbun shimbun-sankei))
  (if (string-match "\\`west\\." (shimbun-current-group-internal shimbun))
      "産経ニュース west"
    (luna-call-next-method)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-sankei)
						 &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      (luna-call-next-method)
    (shimbun-sankei-get-headers shimbun range)))

(defun shimbun-sankei-get-headers (shimbun range)
  "Get headers for non-RSS groups."
  (let* ((group (shimbun-current-group-internal shimbun))
	 (name (shimbun-current-group-name shimbun))
	 (regexp
	  (concat
	   (eval-when-compile
	     (concat
	      "<a[\t\n ]+href=\""
	      ;; 1. url
	      "\\(\\(?:[^\"/]+/\\)+"
	      ;; 2. year
	      "\\([0-9][0-9]\\)"
	      ;; 3. month
	      "\\([01][0-9]\\)"
	      ;; 4. day
	      "\\([0-3][0-9]\\)"
	      "/"
	      ;; 5. serial number
	      "\\([^\"]+\\)"
	      "\\.htm\\)"
	      "\">[\t\n ]*\\(?:【"))
	   ;; 6. subject
	   ;; 7. time
	   name "】\\)?[\t\n ]*\\([^<]+\\)</a>\
\\(?:\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
\\(?:20[0-9][0-9]\\.\\)?[01]?[0-9]\\.[0-3]?[0-9][\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\)[\t\n ]*<\\)?"))
	 (from (concat (shimbun-server-name shimbun) " (" name ")"))
	 (rgrp (mapconcat 'identity (nreverse (split-string group "\\.")) "."))
	 (index (shimbun-index-url shimbun))
	 headers)
    (while (re-search-forward regexp nil t)
      (push (shimbun-create-header
	     0 (match-string 6) from
	     (shimbun-make-date-string
	      (+ 2000 (string-to-number (match-string 2)))
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (match-string 7))
	     (concat "<" (match-string 5) "." rgrp "%"
		     shimbun-sankei-top-level-domain ">")
	     "" 0 0
	     (shimbun-expand-url (match-string 1) index))
	    headers))
    headers))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-sankei)
					    header url)
  (shimbun-sankei-multi-next-url shimbun header url))

(defun shimbun-sankei-multi-next-url (shimbun header url)
  (goto-char (point-min))
  (when (and (re-search-forward "<div[\t\n ]+class=\"pager\"" nil t)
	     (shimbun-end-of-tag "div")
	     (re-search-backward "\
<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*次のページ"
				 (match-beginning 0) t))
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-sankei)
						    header)
  (shimbun-sankei-clear-contents shimbun header))

(defun shimbun-sankei-clear-contents (shimbun header)
  (goto-char (point-min))
  (let ((group (shimbun-current-group-internal shimbun))
	(hankaku (shimbun-japanese-hankaku shimbun))
	(case-fold-search t)
	start end)
    (if (and (or (re-search-forward "<span[\t\n ]+class=\"timestamp\"" nil t)
		 (re-search-forward "<!-+[\t\n ]+grok[\t\n ]+target[\t\n ]+\
title[\t\n ]+end[\t\n ]+-+>"
				    nil t))
	     (progn
	       (setq start (match-end 0))
	       (re-search-forward "<div[\t\n ]+class=\"newstextfull\">[\t\n ]*"
				  nil t))
	     (progn
	       (setq start (if (re-search-backward "<img[\t\n ]+src=\""
						   start t)
			       (match-beginning 0)
			     (match-end 0))) ;; Previous search result
	       (re-search-forward "\
\\(<div[\t\n ]\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"pager\"\\)\
\\|\\(<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"RelatedImg\"\\)\
\\|\\(?:[\t\n ]*<div[^>]*>[\t\n ]*<h[0-9]>[\t\n ]*PR[\t\n ]*</h[0-9]>\\)\
\\|\\(?:[\t\n 　]*<\\(?:/div\\|/?p\\)>\\)*[\t\n 　]*<script[\t\n ]"
				  nil t)))
	(progn
	  (setq end (or (and (match-beginning 2)
			     (save-match-data
			       (and (shimbun-end-of-tag "div")
				    (match-end 0))))
			(match-beginning 0)))
	  (when (prog1
		    (and (match-beginning 1)
			 (shimbun-end-of-tag "div")
			 (re-search-backward "<a[\t\n ]+[^>]+>[^<]*前のページ"
					     end t))
		  (delete-region end (point-max)))
	    (goto-char start)
	    (insert "&#012;\n")) ;; Page delimiter.
	  (delete-region (point-min) start)

	  ;; Insert a new line after every image.
	  (goto-char (point-min))
	  (while (re-search-forward "\\(<img[\t\n ]+[^>]+>\\)[\t\n ]*" nil t)
	    (replace-match "\\1<br>"))

	  ;; Remove useless tags.
	  (goto-char (point-min))
	  (while (re-search-forward "\
\\(?:[\t\n ]*<p\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>[\t\n ]*</p>\\)[\t\n ]*"
				    nil t)
	    (delete-region (match-beginning 0) (match-end 0))
	    (when (string-equal (buffer-substring (max (point-min)
						       (- (point) 6))
						  (point))
				"&#012;")
	      (if (looking-at "<p\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>[\t\n ]*")
		  (replace-match "\n")
		(insert "\n"))))
	  (goto-char (point-min))
	  (when (re-search-forward "\
\\(?:[\t\n ]*</?div\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>\\)+[\t\n ]*\\'"
				   nil t)
	    (delete-region (match-beginning 0) (point-max))
	    (insert "\n"))
	  (goto-char (point-min))
	  (while (re-search-forward "\
\[\t\n ]*\\(?:<div\\(?:[\t\n ]+[^>]+\\)?>[\t\n ]*\\)+\
<span>[\t\n ]*\\[PR\\][\t\n ]*</span>[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*" nil t)
	    (setq start (match-beginning 0)
		  end (match-end 0))
	    (goto-char start)
	    (if (shimbun-end-of-tag "div" t)
		(replace-match "\n")
	      (goto-char end)
	      (when (eobp)
		(delete-region start end)
		(insert "\n"))))
	  (goto-char (point-min))
	  (while (search-forward "<p class=\"zoom\"" nil t)
	    (when (shimbun-end-of-tag "p" t)
	      (delete-region (match-beginning 0) (match-end 0))))
	  (goto-char (point-min))
	  (when (re-search-forward "[\t\n ]*<div id=\"ad2line\"><ul><li>\\'"
				   nil t)
	    (delete-region (match-beginning 0) (match-end 0)))
	  ;; Remove trailing successive orphaned open tags.
	  (goto-char (point-max))
	  (skip-chars-backward "\t\n ")
	  (setq start (point))
	  (while (and (re-search-backward "[\t\n ]*<[^/>][^>]*>" nil t)
		      (or (= (match-end 0) start)
			  (progn
			    (goto-char start)
			    nil)))
	    (setq start (match-beginning 0)))
	  (delete-region (point) (point-max))
	  (insert "\n")

	  (shimbun-remove-orphaned-tag-strips "div\\|span")

	  ;; Convert Japanese zenkaku ASCII chars into hankaku.
	  (when (and hankaku (not (memq hankaku '(header subject))))
	    (shimbun-japanese-hankaku-buffer t))

	  ;; Break long lines.
	  (unless (shimbun-prefer-text-plain-internal shimbun)
	    (shimbun-break-long-japanese-lines))
	  t)
      (erase-buffer)
      (insert "<html><body>\
この記事 (またはこの次のページ) はもうありません。<br>\n\
\(さもなければ通常とは異なる形式を使っているか、<br>\n\
&nbsp;または取得に失敗したのかもしれません。)</body></html>\n")
      nil)))

(provide 'sb-sankei)

;;; sb-sankei.el ends here
