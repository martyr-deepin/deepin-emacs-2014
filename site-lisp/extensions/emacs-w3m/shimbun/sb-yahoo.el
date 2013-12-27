;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007 Kazuyoshi KOREEDA

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yahoo (shimbun) ())

(defvar shimbun-yahoo-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-yahoo-url "http://headlines.yahoo.co.jp/")

(defvar shimbun-yahoo-groups-table
  (let* ((s0 "[\t\n\r ]*")
	 (s1 "[\t\n\r ]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0 "</a>\\(?:" s0 "<[^>]+>\\)+" s0 "（" s0
		    "\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 7. source
		    "\\([^<）]+\\)"
		    s0 "\\(?:</a>" s0 "\\)?"
		    s0 "）" s0 "-" s0 "\\(?:[^<]+)" s0 "\\)?"
		    ;; 8. hour
		    "\\([012]?[0-9]\\)"
		    s0 "時" s0
		    ;; 9. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "分")
		   1 2 3 4 5 6 7 8 9))
	 (topnews (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0 "</a>\\(?:" s0 "<[^>]+>\\)*[^<]*)" s0
		    ;; 7. hour
		    "\\([012]?[0-9]\\)"
		    s0 "時" s0
		    ;; 8. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "分" "[^<]*\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 9. source
		    "\\([^<）]+\\)")
		   1 2 3 4 5 6 9 7 8)))
    `(("topnews" "トップ" "topnews" ,@topnews)
      ("politics" "政治" "pol" ,@default)
      ("society" "社会" "soci" ,@default)
      ("people" "人" "peo" ,@default)
      ("business-all" "経済総合" "bus_all" ,@default)
      ("market" "市況" "brf" ,@default)
      ("stock" "株式" "biz" ,@default)
      ("industry" "産業" "ind" ,@default)
      ("international" "海外" "int" ,@default)
      ("entertainment" "エンターテインメント" "ent" ,@default)
      ("sports" "スポーツ" "spo" ,@default)
      ("computer" "コンピュータ" "sci" ,@default)
      ("zenkoku" "全国" "loc" ,@default)
      ("hokkaido" "北海道" "hok" ,@default)
      ("aomori" "青森" "l02" ,@default) ;; not "102" but "l02" ;-)
      ("iwate" "岩手" "l03" ,@default)
      ("miyagi" "宮城" "l04" ,@default)
      ("akita" "秋田" "l05" ,@default)
      ("yamagata" "山形" "l06" ,@default)
      ("fukushima" "福島" "l07" ,@default)
      ("tokyo" "東京" "l13" ,@default)
      ("kanagawa" "神奈川" "l14" ,@default)
      ("chiba" "千葉" "l12" ,@default)
      ("saitama" "埼玉" "l11" ,@default)
      ("ibaraki" "茨城" "l08" ,@default)
      ("tochigi" "栃木" "l09" ,@default)
      ("gunma" "群馬" "l10" ,@default)
      ("yamanashi" "山梨" "l19" ,@default)
      ("nagano" "長野" "l20" ,@default)
      ("niigata" "新潟" "l15" ,@default)
      ("toyama" "富山" "l16" ,@default)
      ("ishikawa" "石川" "l17" ,@default)
      ("fukui" "福井" "l18" ,@default)
      ("aichi" "愛知" "l23" ,@default)
      ("gifu" "岐阜" "l21" ,@default)
      ("shizuoka" "静岡" "l22" ,@default)
      ("mie" "三重" "l24" ,@default)
      ("osaka" "大阪" "l27" ,@default)
      ("hyogo" "兵庫" "l28" ,@default)
      ("kyoto" "京都" "l26" ,@default)
      ("shiga" "滋賀" "l25" ,@default)
      ("nara" "奈良" "l29" ,@default)
      ("wakayama" "和歌山" "l30" ,@default)
      ("tottori" "鳥取" "l31" ,@default)
      ("shimane" "島根" "l32" ,@default)
      ("okayama" "岡山" "l33" ,@default)
      ("hiroshima" "広島" "l34" ,@default)
      ("yamaguchi" "山口" "l35" ,@default)
      ("tokushima" "徳島" "l36" ,@default)
      ("kagawa" "香川" "l37" ,@default)
      ("ehime" "愛媛" "l38" ,@default)
      ("kochi" "高知" "l39" ,@default)
      ("fukuoka" "福岡" "l40" ,@default)
      ("saga" "佐賀" "l41" ,@default)
      ("nagasaki" "長崎" "l42" ,@default)
      ("kumamoto" "熊本" "l43" ,@default)
      ("oita" "大分" "l44" ,@default)
      ("miyazaki" "宮崎" "l45" ,@default)
      ("kagoshima" "鹿児島" "l46" ,@default)
      ("okinawa" "沖縄" "oki" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where numbers point to the search result in order
of [0]a url, [1]a serial number, [2]a year, [3]a month, [4]a day,
\[5]a subject, [6]a news source, [7]an hour and [8]a minute.")

(defvar shimbun-yahoo-groups
  (mapcar 'car shimbun-yahoo-groups-table))

(defvar shimbun-yahoo-from-address "nobody@example.com")
(defvar shimbun-yahoo-content-start "<!---記事-->\
\\(?:[\t\n ]*<h[0-9][^>]*>[^[<]+</h[0-9]>[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
\[^<]+[0-9]分配信[^<]*<a[\t\n ]+href=[^>]+>[^<]+</a>\\)?\
\\(?:[\t\n ]*<[^>i][^>]*>\\)*[\t\n ]*")
(defvar shimbun-yahoo-content-end "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
\\(?:最終更新:[01]?[0-9]月\\|<!---コブランド-->\\|<!---/記事-->\\)")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
\(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
;;;<DEBUG>
;;  (shimbun-yahoo-index-url shimbun))
;;
;;(defun shimbun-yahoo-index-url (shimbun)
;;;</DEBUG>
  (format "%shl?c=%s&t=l"
	  (shimbun-url-internal shimbun)
	  (nth 2 (assoc (shimbun-current-group-internal shimbun)
			shimbun-yahoo-groups-table))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-yahoo-get-headers shimbun range))
;;
;;(defun shimbun-yahoo-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (from "Yahoo!ニュース")
	 (group (shimbun-current-group-internal shimbun))
	 (numbers (cdr (assoc group shimbun-yahoo-groups-table)))
	 (jname (pop numbers))
	 (regexp (progn (setq numbers (cdr numbers)) (pop numbers)))
	 (pages (shimbun-header-index-pages range))
	 (count 0)
	 (index (shimbun-index-url shimbun))
	 id headers start)
    (catch 'stop
      (while t
	(shimbun-remove-tags "<!-+[\t\n ]*アクセスランキング[\t\n ]*-+>"
			     "<!-+[\t\n ]*/アクセスランキング[\t\n ]*-+>")
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (setq id (concat "<"
			   (save-match-data
			     (shimbun-replace-in-string
			      (match-string (nth 1 numbers))
			      "-" "."))
			   "%" group ".headlines.yahoo.co.jp>"))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	  (push (shimbun-create-header
		 0
		 (match-string (nth 5 numbers))
		 (concat from " (" jname "/" (match-string (nth 6 numbers))
			 ")")
		 (shimbun-make-date-string
		  (string-to-number (match-string (nth 2 numbers)))
		  (string-to-number (match-string (nth 3 numbers)))
		  (string-to-number (match-string (nth 4 numbers)))
		  (format "%02d:%02d"
			  (string-to-number (match-string (nth 7 numbers)))
			  (string-to-number (match-string (nth 8 numbers)))))
		 id "" 0 0
		 (match-string (nth 0 numbers)))
		headers))
	(goto-char (point-min))
	(if (re-search-forward "<a href=\"\\([^\"]+\\)\">次のページ</a>" nil t)
	    (shimbun-retrieve-url (prog1
				      (match-string 1)
				    (erase-buffer))
				  t)
	  (if (and (or (not pages)
		       (< (setq count (1+ count)) pages))
		   (re-search-forward "<!-+[\t\n ]*過去記事[\t\n ]*-+>" nil t)
		   (progn
		     (setq start (match-end 0))
		     (re-search-forward "<!-+[\t\n ]*/過去記事[\t\n ]*-+>"
					nil t))
		   (progn
		     (narrow-to-region start (match-beginning 0))
		     (goto-char start)
		     (or (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]+selected[\t\n ]*>"
					    nil t)
			 (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]*>"
					    nil t)))
		   (re-search-forward "<option[\t\n ]+value=\"\
\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\"[\t\n ]*>"
				      nil t))
	      (shimbun-retrieve-url (prog1
					(concat index "&d=" (match-string 1))
				      (erase-buffer))
				    t)
	    (throw 'stop nil)))))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-yahoo)
						   header)
;;;<DEBUG>
;;  (shimbun-yahoo-prepare-article shimbun header))
;;
;;(defun shimbun-yahoo-prepare-article (shimbun header)
;;;</DEBUG>
  ;; Remove headline.
  (shimbun-remove-tags "<h[0-9][\t\n ]+class=\"yjXL\">" "</h[0-9]>")
  (shimbun-remove-tags
   "<p[\t\n ]+class=\"yjSt\">[^<]*[0-9]+時[0-9]+分配信" "</p>")
  ;; Remove garbage.
  (when (re-search-forward "\
\[\t\n ]*<p[\t\n ]+class=\"yjSt\">[\t\n ]*拡大写真[\t\n ]*</p>[\t\n ]*"
			   nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (shimbun-with-narrowed-article
   shimbun
   ;; Fix the picture tag.
   (cond ((re-search-forward "[\t\n ]*<center>[\t\n ]*<font[^>]+>\
\[\t\n ]*拡大写真[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)+"
			     nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (when (and (shimbun-prefer-text-plain-internal shimbun)
		     (looking-at "[^<>]+"))
	    (replace-match "(写真: \\&)<br>"))
	  (goto-char (point-min)))
	 ((and (shimbun-prefer-text-plain-internal shimbun)
	       (re-search-forward "<img[\t\n ]+[^>]+>\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<font[\t\n ]+[^>]+>[\t\n 　]*\
\\([^<>]+\\)[\t\n ]*</font>"
				  nil t))
	  (if (string-equal (match-string 1) "写真")
	      (replace-match "(写真)<br>")
	    (replace-match "(写真: \\1)<br>"))))
   (if (shimbun-prefer-text-plain-internal shimbun)
       (require 'sb-text) ;; `shimbun-fill-column'
     ;; Open paragraphs.
     (while (re-search-forward "。<br>[\t ]*\n　" nil t)
       (replace-match "。<br><br>\n　"))
     (goto-char (point-min)))
   ;; Correct the Date header and the position of the footer.
   (let (year footer)
     (when (and
	    (setq year (shimbun-header-date header))
	    (string-match " \\(20[0-9][0-9]\\) " year)
	    (progn
	      (setq year (string-to-number (match-string 1 year)))
	      (re-search-forward
	       (eval-when-compile
		 (let ((s0 "[\t\n ]*")
		       (s1 "[\t\n ]+"))
		   (concat
		    "[\t\n 　]*<div" s1 "align=right>" s0
		    ;; 1. footer
		    "\\("
		    "（[^）]+）" s1 "-" s1
		    ;; 2. month
		    "\\([01]?[0-9]\\)"
		    s0 "月" s0
		    ;; 3. day
		    "\\([0-3]?[0-9]\\)"
		    s0 "日" s0
		    ;; 4. hour
		    "\\([012]?[0-9]\\)"
		    s0 "時" s0
		    ;; 5. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "分" s0 "更新"
		    "\\)"
		    s0 "</div>\\(?:" s0 "<br>\\)*")))
	       nil t)))
       (shimbun-header-set-date
	header
	(shimbun-make-date-string
	 year
	 (string-to-number (match-string 2))
	 (string-to-number (match-string 3))
	 (format "%02d:%02d"
		 (string-to-number (match-string 4))
		 (string-to-number (match-string 5)))))
       (setq footer (match-string 1))
       (delete-region (match-beginning 0) (match-end 0))
       (if (shimbun-prefer-text-plain-internal shimbun)
	   (insert "<br><br>"
		   (make-string (max (- (symbol-value 'shimbun-fill-column)
					(string-width footer))
				     0)
				? )
		   footer "<br>")
	 (insert "<br><br><div align=right>" footer "</div>")
	 ;; Break long Japanese lines.
	 (goto-char (point-min))
	 (while (re-search-forward "<p[^>]*>\\|</p>\\|[、。）」]+" nil t)
	   (unless (eolp)
	     (insert "\n"))))))))

(provide 'sb-yahoo)

;;; sb-yahoo.el ends here
