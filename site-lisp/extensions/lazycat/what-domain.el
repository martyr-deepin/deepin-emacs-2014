;;; what-domain.el --- top-level internet domain name information

;; Copyright (C) 2001 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 2001-09-20

;; $Id: what-domain.el,v 1.1 2001/09/21 03:26:13 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For several years there has been a command `what-domain' defined in
;; mail-extr.el, which is shipped with Emacs.  But it has several
;; limitations:
;;
;;     * It's not up to date in older versions of emacs
;;     * Only resolves 2-letter country codes into domain names
;;     * Not callable from lisp because it always writes to the echo area
;;
;; This package provides the following commands:
;;
;;     * what-domain-a2       (`what-domain' is aliased to this)
;;     * what-domain-a3       (lets you query 3-letter country codes)
;;     * what-domain-num      (lets you query numeric country codes)
;;     * what-domain-apropos  (lets you search for domain codes by name)
;;
;; Each of which returns a useful list of information when called from
;; lisp, and allows you to perform interactive searches in different ways.

;; Most of the country codes come from the RIPE Network Coordination
;; Centre's tracking of ISO 3166, last updated 1997-08-07 15:59:51 UTC

;;; Code:

(defvar what-domain-list
  '(
    ;; A2   A3   Num   Country                 Name
    ("AD" "AND" "020" "Andorra")
    ("AE" "ARE" "784" "United Arab Emirates")
    ("AF" "AFG" "004" "Afghanistan")
    ("AG" "ATG" "028" "Antigua and Barbuda")
    ("AI" "AIA" "660" "Anguilla")
    ("AL" "ALB" "008" "Albania")
    ("AM" "ARM" "051" "Armenia")
    ("AN" "ANT" "530" "Netherlands Antilles")
    ("AO" "AGO" "024" "Angola")
    ("AQ" "ATA" "010" "Antarctica")
    ("AR" "ARG" "032" "Argentina"             "Argentine Republic")
    ("AS" "ASM" "016" "American Samoa")
    ("AT" "AUT" "040" "Austria"               "The Republic of %s")
    ("AU" "AUS" "036" "Australia")
    ("AW" "ABW" "533" "Aruba")
    ("AZ" "AZE" "031" "Azerbaijan")
    ("BA" "BIH" "070" "Bosnia and Herzegowina")
    ("BB" "BRB" "052" "Barbados")
    ("BD" "BGD" "050" "Bangladesh")
    ("BE" "BEL" "056" "Belgium"               "The Kingdom of %s")
    ("BF" "BFA" "854" "Burkina Faso")
    ("BG" "BGR" "100" "Bulgaria")
    ("BH" "BHR" "048" "Bahrain")
    ("BI" "BDI" "108" "Burundi")
    ("BJ" "BEN" "204" "Benin")
    ("BM" "BMU" "060" "Bermuda")
    ("BN" "BRN" "096" "Brunei Darussalam")
    ("BO" "BOL" "068" "Bolivia"               "Republic of %s")
    ("BR" "BRA" "076" "Brazil"                "The Federative Republic of %s")
    ("BS" "BHS" "044" "Bahamas")
    ("BT" "BTN" "064" "Bhutan")
    ("BV" "BVT" "074" "Bouvet Island")
    ("BW" "BWA" "072" "Botswana")
    ("BY" "BLR" "112" "Belarus")
    ("BZ" "BLZ" "084" "Belize")
    ("CA" "CAN" "124" "Canada")
    ("CC" "CCK" "166" "Cocos (Keeling) Islands")
    ("CD" "COD" "180" "Congo"                 "The Democratic Republic of the %s")
    ("CF" "CAF" "140" "Central African Republic")
    ("CG" "COG" "178" "Congo")
    ("CH" "CHE" "756" "Switzerland"           "The Swiss Confederation")
    ("CI" "CIV" "384" "Cote D'Ivoire"         "Ivory Coast")
    ("CK" "COK" "184" "Cook Islands")
    ("CL" "CHL" "152" "Chile"                 "The Republic of %s")
    ("CM" "CMR" "120" "Cameroon")
    ("CN" "CHN" "156" "China"                 "The People's Republic of %s")
    ("CO" "COL" "170" "Colombia")
    ("CR" "CRI" "188" "Costa Rica"            "The Republic of %s")
    ("CU" "CUB" "192" "Cuba")
    ("CV" "CPV" "132" "Cape Verde")
    ("CX" "CXR" "162" "Christmas Island")
    ("CY" "CYP" "196" "Cyprus")
    ("CZ" "CZE" "203" "Czech Republic")
    ("DE" "DEU" "276" "Germany")
    ("DJ" "DJI" "262" "Djibouti")
    ("DK" "DNK" "208" "Denmark")
    ("DM" "DMA" "212" "Dominica")
    ("DO" "DOM" "214" "Dominican Republic"    "The %s")
    ("DZ" "DZA" "012" "Algeria")
    ("EC" "ECU" "218" "Ecuador"               "The Republic of %s")
    ("EE" "EST" "233" "Estonia")
    ("EG" "EGY" "818" "Egypt"                 "The Arab Republic of %s")
    ("EH" "ESH" "732" "Western Sahara")
    ("ER" "ERI" "232" "Eritrea")
    ("ES" "ESP" "724" "Spain"                 "The Kingdom of %s")
    ("ET" "ETH" "231" "Ethiopia")
    ("FI" "FIN" "246" "Finland"               "The Republic of %s")
    ("FJ" "FJI" "242" "Fiji")
    ("FK" "FLK" "238" "Falkland Islands (Malvinas)")
    ("FM" "FSM" "583" "Micronesia"            "Federated States of %s")
    ("FO" "FRO" "234" "Faroe Islands")
    ("FR" "FRA" "250" "France")
    ("FX" "FXX" "249" "France"                "Metropolitan %s")
    ("GA" "GAB" "266" "Gabon")
    ("GB" "GBR" "826" "United Kingdom")
    ("GD" "GRD" "308" "Grenada")
    ("GE" "GEO" "268" "Georgia")
    ("GF" "GUF" "254" "French Guiana")
    ("GH" "GHA" "288" "Ghana")
    ("GI" "GIB" "292" "Gibraltar")
    ("GL" "GRL" "304" "Greenland")
    ("GM" "GMB" "270" "Gambia")
    ("GN" "GIN" "324" "Guinea")
    ("GP" "GLP" "312" "Guadeloupe"            "%s (Fr.)")
    ("GQ" "GNQ" "226" "Equatorial Guinea")
    ("GR" "GRC" "300" "Greece"                "The Hellenic Republic (%s)")
    ("GS" "SGS" "239" "South Georgia and the South Sandwich Islands")
    ("GT" "GTM" "320" "Guatemala")
    ("GU" "GUM" "316" "Guam"                  "%s (U.S.)")
    ("GW" "GNB" "624" "Guinea-Bissau")
    ("GY" "GUY" "328" "Guyana")
    ("HK" "HKG" "344" "Hong Kong")
    ("HM" "HMD" "334" "Heard and Mc Donald Islands")
    ("HN" "HND" "340" "Honduras")
    ("HR" "HRV" "191" "Croatia"               "%s (Hrvatska)")
    ("HT" "HTI" "332" "Haiti")
    ("HU" "HUN" "348" "Hungary"               "The Hungarian Republic")
    ("ID" "IDN" "360" "Indonesia")
    ("IE" "IRL" "372" "Ireland")
    ("IL" "ISR" "376" "Israel"                "The State of %s")
    ;; NOT in ISO 3166-1 of 2001-02-26
    ("IM" nil   nil   "Isle of Man"           "The %s")
    ("IN" "IND" "356" "India"                 "The Republic of %s")
    ("IO" "IOT" "086" "British Indian Ocean Territory")
    ("IQ" "IRQ" "368" "Iraq")
    ("IR" "IRN" "364" "Iran"                  "Islamic Republic of %s")
    ("IS" "ISL" "352" "Iceland"               "The Republic of %s")
    ("IT" "ITA" "380" "Italy"                 "The Italian Republic")
    ("JM" "JAM" "388" "Jamaica")
    ("JO" "JOR" "400" "Jordan")
    ("JP" "JPN" "392" "Japan")
    ("KE" "KEN" "404" "Kenya")
    ("KG" "KGZ" "417" "Kyrgyzstan")
    ("KH" "KHM" "116" "Cambodia")
    ("KI" "KIR" "296" "Kiribati")
    ("KM" "COM" "174" "Comoros")
    ("KN" "KNA" "659" "Saint Kitts and Nevis")
    ("KP" "PRK" "408" "Korea (North)"         "Democratic People's Republic of %s")
    ("KR" "KOR" "410" "Korea (South)"         "Republic of %s")
    ("KW" "KWT" "414" "Kuwait")
    ("KY" "CYM" "136" "Cayman Islands")
    ("KZ" "KAZ" "398" "Kazakhstan")
    ("LA" "LAO" "418" "Lao People's Democratic Republic")
    ("LB" "LBN" "422" "Lebanon")
    ("LC" "LCA" "662" "Saint Lucia")
    ("LI" "LIE" "438" "Liechtenstein")
    ("LK" "LKA" "144" "Sri Lanka"             "The Democratic Socialist Republic of %s")
    ("LR" "LBR" "430" "Liberia")
    ("LS" "LSO" "426" "Lesotho")
    ("LT" "LTU" "440" "Lithuania")
    ("LU" "LUX" "442" "Luxembourg")
    ("LV" "LVA" "428" "Latvia")
    ("LY" "LBY" "434" "Libyan Arab Jamahiriya")
    ("MA" "MAR" "504" "Morocco")
    ("MC" "MCO" "492" "Monaco")
    ("MD" "MDA" "498" "Moldova"               "The Republic of %s")
    ("MG" "MDG" "450" "Madagascar")
    ("MH" "MHL" "584" "Marshall Islands")
    ("MK" "MKD" "807" "Macedonia"             "The Former Yugoslav Republic of %s")
    ("ML" "MLI" "466" "Mali")
    ("MM" "MMR" "104" "Myanmar")
    ("MN" "MNG" "496" "Mongolia")
    ("MO" "MAC" "446" "Macau")
    ("MP" "MNP" "580" "Northern Mariana Islands")
    ("MQ" "MTQ" "474" "Martinique")
    ("MR" "MRT" "478" "Mauritania")
    ("MS" "MSR" "500" "Montserrat")
    ("MT" "MLT" "470" "Malta")
    ("MU" "MUS" "480" "Mauritius")
    ("MV" "MDV" "462" "Maldives")
    ("MW" "MWI" "454" "Malawi")
    ("MX" "MEX" "484" "Mexico"                "The United Mexican States")
    ("MY" "MYS" "458" "Malaysia"              "%s (changed to Myanmar?)")
    ("MZ" "MOZ" "508" "Mozambique")
    ("NA" "NAM" "516" "Namibia")
    ("NC" "NCL" "540" "New Caledonia"         "%s (Fr.)")
    ("NE" "NER" "562" "Niger")
    ("NF" "NFK" "574" "Norfolk Island")
    ("NG" "NGA" "566" "Nigeria")
    ("NI" "NIC" "558" "Nicaragua"             "The Republic of %s")
    ("NL" "NLD" "528" "Netherlands"           "The Kingdom of the %s")
    ("NO" "NOR" "578" "Norway"                "The Kingdom of %s")
    ("NP" "NPL" "524" "Nepal")
    ("NR" "NRU" "520" "Nauru")
    ("NU" "NIU" "570" "Niue")
    ("NZ" "NZL" "554" "New Zealand")
    ("OM" "OMN" "512" "Oman")
    ("PA" "PAN" "591" "Panama")
    ("PE" "PER" "604" "Peru")
    ("PF" "PYF" "258" "French Polynesia")
    ("PG" "PNG" "598" "Papua New Guinea")
    ("PH" "PHL" "608" "Philippines"           "The Republic of the %s")
    ("PK" "PAK" "586" "Pakistan")
    ("PL" "POL" "616" "Poland")
    ("PM" "SPM" "666" "St. Pierre and Miquelon")
    ("PN" "PCN" "612" "Pitcairn")
    ("PR" "PRI" "630" "Puerto Rico"           "%s (U.S.)")
    ("PS" nil   nil   "Palestinian Territory" "Occupied %s")
    ("PT" "PRT" "620" "Portugal"              "The Portuguese Republic")
    ("PW" "PLW" "585" "Palau")
    ("PY" "PRY" "600" "Paraguay")
    ("QA" "QAT" "634" "Qatar")
    ("RE" "REU" "638" "Reunion"               "%s (Fr.)")
    ("RO" "ROM" "642" "Romania")
    ("RU" "RUS" "643" "Russian Federation")
    ("RW" "RWA" "646" "Rwanda")
    ("SA" "SAU" "682" "Saudi Arabia")
    ("SB" "SLB" "090" "Solomon Islands")
    ("SC" "SYC" "690" "Seychelles")
    ("SD" "SDN" "736" "Sudan")
    ("SE" "SWE" "752" "Sweden"                "The Kingdom of %s")
    ("SG" "SGP" "702" "Singapore"             "The Republic of %s")
    ("SH" "SHN" "654" "St. Helena")
    ("SI" "SVN" "705" "Slovenia")
    ("SJ" "SJM" "744" "Svalbard and Jan Mayen Islands")
    ("SK" "SVK" "703" "Slovakia"              "The Slovak Republic")
    ("SL" "SLE" "694" "Sierra Leone")
    ("SM" "SMR" "674" "San Marino")
    ("SN" "SEN" "686" "Senegal")
    ("SO" "SOM" "706" "Somalia")
    ("SR" "SUR" "740" "Suriname")
    ("ST" "STP" "678" "Sao Tome and Principe")
    ;; Obsolete
    ("SU" nil   nil   "U.S.S.R."              "The Union of Soviet Socialist Republics")
    ("SV" "SLV" "222" "El Salvador")
    ("SY" "SYR" "760" "Syrian Arab Republic")
    ("SZ" "SWZ" "748" "Swaziland")
    ("TC" "TCA" "796" "Turks and Caicos Islands")
    ("TD" "TCD" "148" "Chad")
    ("TF" "ATF" "260" "French Southern Territories")
    ("TG" "TGO" "768" "Togo")
    ("TH" "THA" "764" "Thailand"              "The Kingdom of %s")
    ("TJ" "TJK" "762" "Tajikistan")
    ("TK" "TKL" "772" "Tokelau")
    ("TM" "TKM" "795" "Turkmenistan")
    ("TN" "TUN" "788" "Tunisia")
    ("TO" "TON" "776" "Tonga")
    ("TP" "TMP" "626" "East Timor")
    ("TR" "TUR" "792" "Turkey"                "The Republic of %s")
    ("TT" "TTO" "780" "Trinidad And Tobago")
    ("TV" "TUV" "798" "Tuvalu")
    ("TW" "TWN" "158" "Taiwan"                "%s, Province of China")
    ("TZ" "TZA" "834" "Tanzania"              "United Republic of %s")
    ("UA" "UKR" "804" "Ukraine")
    ("UG" "UGA" "800" "Uganda")
    ("UK" nil   nil   "United Kingdom"	      "The %s of Great Britain and Northern Ireland")
    ("UM" "UMI" "581" "United States Minor Outlying Islands")
    ("US" "USA" "840" "United States"         "The %s of America")
    ("UY" "URY" "858" "Uruguay"               "The Eastern Republic of %s")
    ("UZ" "UZB" "860" "Uzbekistan")
    ("VA" "VAT" "336" "Holy See (Vatican City State)")
    ("VC" "VCT" "670" "Saint Vincent and the Grenadines")
    ("VE" "VEN" "862" "Venezuela"             "The Republic of %s")
    ("VG" "VGB" "092" "Virgin Islands"        "%s (British)")
    ("VI" "VIR" "850" "Virgin Islands"        "%s (U.S.)")
    ("VN" "VNM" "704" "Viet Nam"              "Vietnam")
    ("VU" "VUT" "548" "Vanuatu")
    ("WF" "WLF" "876" "Wallis and Futuna Islands")
    ("WS" "WSM" "882" "Samoa")
    ("YE" "YEM" "887" "Yemen")
    ("YT" "MYT" "175" "Mayotte")
    ("YU" "YUG" "891" "Yugoslavia"            "%s (Serbia-Montenegro)")
    ("ZA" "ZAF" "710" "South Africa"          "The Republic of %s")
    ("ZM" "ZMB" "894" "Zambia")
    ("ZW" "ZWE" "716" "Zimbabwe"              "Republic of %s")

    ;; Special top-level domains:
    ("ARPA"   nil nil "Advanced Research Projects Agency (U.S. DoD)")
    ("BITNET" nil nil "Because It's Time NET")
    ("COM"    nil nil "Commercial")
    ("EDU"    nil nil "Educational")
    ("GOV"    nil nil "Government (U.S.)")
    ("INT"    nil nil "International (NATO)")
    ("MIL"    nil nil "Military (U.S.)")
    ("NATO"   nil nil "North Atlantic Treaty Organization")
    ("NET"    nil nil "Network")
    ("ORG"    nil nil "Non-profit Organization")
    ("UUCP"   nil nil "Unix to Unix CoPy"))
  "ISO 3166 codes (plus a few extra).")

(defvar what-domain-obarray-size 211)

;; Each elt has 3 values: key offset obarray
;; The 3rd elt of each slot is initialized with an index (obarray) of the
;; above list the first time a query on it is made.
(defvar what-domain-table
  '((a2      0 nil)
    (a3      1 nil)
    (num     2 nil)
    (country 3 nil)))


(defun what-domain-make-index (key-index)
  (let ((tbl what-domain-list)
        (ob (make-vector what-domain-obarray-size 0))
        l sym key)
    (while tbl
      (setq l (car tbl)
            tbl (cdr tbl))
      (setq key (nth key-index l))
      (cond (key
             (setq sym (intern key ob))
             (or (= key-index 0) (put sym 'a2  (nth 0 l)))
             (or (= key-index 1) (put sym 'a3  (nth 1 l)))
             (or (= key-index 2) (put sym 'num (nth 2 l)))
             (put sym 'name (if (nth 4 l)
                                (format (nth 4 l) (nth 3 l))
                              (nth 3 l)))

             ;; Forward "1" -> "001" for completion convenience
             (and (= key-index 2)
                  ;; (let ((q (int-to-string (string-to-int key))))
                  (let ((q (int-to-string (string-to-number key))))
                    (or (string= q key)
                        (set (intern q ob) key)))))))
    ob))

(defun what-domain-get-index (name)
  (let ((slot (assq name what-domain-table)))
    (or (nth 2 slot)
        (setcar (nthcdr 2 slot) (what-domain-make-index (nth 1 slot))))))

(defun what-domain-get-key (key index)
  (intern-soft (if (symbolp key)
                   (upcase (symbol-name key))
                 (upcase key))
               (if (vectorp index)
                   index
                 (what-domain-get-index index))))

(defun what-domain-get (key prop index)
  (let ((sym (what-domain-get-key key index)))
    ;; If key indicates forwarding, get forwarded key.
    ;; This is probably only used in the `num' index.
    (while (and sym (boundp sym))
      (setq sym (what-domain-get-key (symbol-value sym) index)))
    (get sym prop)))

(defun what-domain-completing-read (prompt table)
  (let ((tbl (what-domain-get-index table))
        (completion-ignore-case t))
    (completing-read prompt tbl nil t)))


;;;###autoload
(defun what-domain-a2 (a2-code)
  "Return domain name for corresponding 2-letter country code.
If called interactively, display name in echo area."
  (interactive (list (what-domain-completing-read
                      "2-letter country code: " 'a2)))
  (setq a2-code (upcase a2-code))
  (let ((name (what-domain-get a2-code 'name 'a2)))
    (if (interactive-p)
        (if name
            (message "%s: %s" a2-code name)
          (message "%s: unknown A2 code" a2-code)))
    name))

;;;###autoload
(defun what-domain-a3 (a3-code)
  "Return domain name for corresponding 3-letter country code.
If called interactively, display name in echo area."
  (interactive (list (what-domain-completing-read
                      "3-letter country code: " 'a3)))
  (setq a3-code (upcase a3-code))
  (let ((name (what-domain-get a3-code 'name 'a3)))
    (if (interactive-p)
        (if name
            (message "%s: %s" a3-code name)
          (message "%s: unknown A3 code" a3-code)))
    name))

;;;###autoload
(defun what-domain-num (num-code)
  "Return domain name for corresponding 3-digit numeric country code.
If called interactively, display name in echo area."
  (interactive (list (what-domain-completing-read
                      "3-digit numeric country code: " 'num)))
  (setq num-code (upcase num-code))
  (let ((name (what-domain-get num-code 'name 'num)))
    (if (interactive-p)
        (if name
            (message "(%s) %s: %s"
                     num-code
                     (what-domain-get num-code 'a2 'num)
                     name)
          (message "%s: unknown numeric country code" num-code)))
    name))

(defun what-domain-apropos (regexp)
  "Return country codes and full domain name for any domain matching REGEXP.
If called interactively, display 2-letter country code and full name in the
echo area (if 1 result) or a temporary buffer (if more than 1 result)."
  (interactive "sDomain name regexp: ")
  (let ((case-fold-search t)
        (found nil))
    (save-match-data
      (mapatoms #'(lambda (s)
                    (and (string-match regexp (get s 'name))
                         (setq found (cons (list (get s 'a2)
                                                 (get s 'a3)
                                                 (get s 'num)
                                                 (get s 'name))
                                           found))))
                (what-domain-get-index 'country)))
    (and found
         (cdr found)
         (setq found (sort found #'(lambda (a b)
                                     (string-lessp (car a) (car b))))))
    (cond ((not (interactive-p)))
          ((null found)
           (message "No matches"))
          ((null (cdr found))
           (message "%s: %s" (nth 0 (car found)) (nth 3 (car found))))
          (t
           (with-output-to-temp-buffer "*Domain Apropos*"
             (save-excursion
               (set-buffer standard-output)
               (let ((f found))
                 (while f
                   (insert (format "%s: %s\n" (nth 0 (car f)) (nth 3 (car f))))
                   (setq f (cdr f))))))))
    found))

;;;###autoload
(defalias 'what-domain 'what-domain-a2)

(provide 'what-domain)

;;; what-domain.el ends here
