;; -*-coding: utf-8 -*-
;;; nterm-ut.el --- nterm unit testing

;;; Commentary:

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;;; Code:
(require 'ert-ui)
(require 'nterm)

(defun nterm-ut-checksum ()
  "Display screen and memory checksum.
It places the result in the kill ring."
  (interactive)
  (let ((string (format "\"%s\" \"%s\""
                        (md5 (get-buffer nterm-buffer-name))
                        (nterm-mem-checksum))))
    (kill-new string)
    (message string)))

(defun nterm-ut-init ()
  "Init nterm for unit testing"
  (if (get-buffer nterm-buffer-name)
      (kill-buffer nterm-buffer-name))
  (setq nterm-unit-testing t)
  (nterm-mode))


(defun nterm-ut (md5-buf md5-mem record)
  "Emulate sequence RECORD then checksum screen and memory."
  (nterm-ut-init)
  (nterm-emulate nil record)
  (setq nterm-unit-testing nil)
  (and
   (string= (md5 (get-buffer nterm-buffer-name)) md5-buf)
   (string= (nterm-mem-checksum) md5-mem)))


;;; Test of cursor movements
(ert-deftest nterm-vttest-1-1 ()
  (should (nterm-ut 
"8752c42bb037e167ca4d0b5c1eaa3806" "2601cc448dbfd675b3a32619b3791995"
 (concat
"[2J[?3l#8[9;10H[1J"
"[18;60H[0J[1K[9;71H[0K[10;10H[1K[10;71H[0K[11;10H"
"[1K[11;71H[0K[12;10H[1K[12;71H[0K[13;10H[1K[13;71H"
"[0K[14;10H[1K[14;71H[0K[15;10H[1K[15;71H[0K[16;10H"
"[1K[16;71H[0K[17;30H[2K[24;1f*[1;1f*[24;2f*[1;2f*[2"
"4;3f*[1;3f*[24;4f*[1;4f*[24;5f*[1;5f*[24;6f*[1;6f*[2"
"4;7f*[1;7f*[24;8f*[1;8f*[24;9f*[1;9f*[24;10f*[1;10f*"
"[24;11f*[1;11f*[24;12f*[1;12f*[24;13f*[1;13f*[24;14f*"
"[1;14f*[24;15f*[1;15f*[24;16f*[1;16f*[24;17f*[1;17f*["
"24;18f*[1;18f*[24;19f*[1;19f*[24;20f*[1;20f*[24;21f*["
"1;21f*[24;22f*[1;22f*[24;23f*[1;23f*[24;24f*[1;24f*[2"
"4;25f*[1;25f*[24;26f*[1;26f*[24;27f*[1;27f*[24;28f*[1"
";28f*[24;29f*[1;29f*[24;30f*[1;30f*[24;31f*[1;31f*[24"
";32f*[1;32f*[24;33f*[1;33f*[24;34f*[1;34f*[24;35f*[1;"
"35f*[24;36f*[1;36f*[24;37f*[1;37f*[24;38f*[1;38f*[24;"
"39f*[1;39f*[24;40f*[1;40f*[24;41f*[1;41f*[24;42f*[1;4"
"2f*[24;43f*[1;43f*[24;44f*[1;44f*[24;45f*[1;45f*[24;4"
"6f*[1;46f*[24;47f*[1;47f*[24;48f*[1;48f*[24;49f*[1;49"
"f*[24;50f*[1;50f*[24;51f*[1;51f*[24;52f*[1;52f*[24;53"
"f*[1;53f*[24;54f*[1;54f*[24;55f*[1;55f*[24;56f*[1;56f"
"*[24;57f*[1;57f*[24;58f*[1;58f*[24;59f*[1;59f*[24;60f"
"*[1;60f*[24;61f*[1;61f*[24;62f*[1;62f*[24;63f*[1;63f*"
"[24;64f*[1;64f*[24;65f*[1;65f*[24;66f*[1;66f*[24;67f*"
"[1;67f*[24;68f*[1;68f*[24;69f*[1;69f*[24;70f*[1;70f*"
"[24;71f*[1;71f*[24;72f*[1;72f*[24;73f*[1;73f*[24;74f*"
"[1;74f*[24;75f*[1;75f*[24;76f*[1;76f*[24;77f*[1;77f*["
"24;78f*[1;78f*[24;79f*[1;79f*[24;80f*[1;80f*[2;2H+[1D"
"D+[1DD+[1DD+[1DD+[1DD+[1DD+[1DD+[1DD+[1DD+"
"[1DD+[1DD+[1DD+[1DD+[1DD+[1DD+[1DD+[1DD+[1D"
"D+[1DD+[1DD+[1DD+[1DD[23;79H+[1DM+[1DM+[1DM+"
"[1DM+[1DM+[1DM+[1DM+[1DM+[1DM+[1DM+[1DM+[1D"
"M+[1DM+[1DM+[1DM+[1DM+[1DM+[1DM+[1DM+[1DM+["
"1DM+[1DM[2;1H*[2;80H*[10DE*[3;80H*[10DE*[4;80H*["
"10DE*[5;80H*[10DE*[6;80H*[10DE*[7;80H*[10DE*[8;80"
"H*[10DE*[9;80H*[10DE*[10;80H*[10D*[11;80H*[10D*"
"[12;80H*[10D*[13;80H*[10D*[14;80H*[10D*[15;80H*"
"[10D*[16;80H*[10D*[17;80H*[10D*[18;80H*[10D*"
"[19;80H*[10D*[20;80H*[10D*[21;80H*[10D*[22;80H*"
"[10D*[23;80H*[10D[2;10H[42D[2C+[0C[2D[1C+[0C[2"
"D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+["
"0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D["
"1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C"
"[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+"
"[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D"
"[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0"
"C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1"
"C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C["
"2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+"
"[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D"
"[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C"
"[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C"
"+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2"
"D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+["
"0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D["
"1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C[2D[1C+[0C"
"[2D[1C[23;70H[42C[2D+[1D[1C[0D+[1D[1C[0D+[1D[1"
"C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0"
"D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+"
"[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D"
"[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C"
"[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D"
"+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1"
"D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1"
"C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0"
"D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+"
"[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D"
"[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C"
"[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D"
"+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1"
"D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1"
"C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0"
"D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+"
"[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D[1C[0D+[1D"
"[1C[0D[1;1H[10A[1A[0A[24;80H[10B[1B[0B[10;12H    "
"                                                      [1B["
"58D                                                         "
" [1B[58D                                                  "
"        [1B[58D                                           "
"               [1B[58D                                    "
"                      [1B[58D                             "
"                             [1B[58D[5A[1CThe screen sho"
"uld be cleared,  and have an unbroken bor-[12;13Hder of *'s"
" and +'s around the edge,   and exactly in the[13;13Hmiddle"
"  there should be a frame of E's around this  text[14;13Hwi"
"th  one (1) free position around it.    Push <RETURN>"))))

(ert-deftest nterm-vttest-1-3 ()
  (should (nterm-ut 
"3e98ebe35b09b63d405b209dbc53c8d4" "1ab768a2b495fa6e0afac3fb8457fc59"
 (concat
"[?7h[?3l[?"
"3lTest of autowrap, mixing control and print characters.T"
"he left/right margins should have letters in order:[3;21"
"r[?6h[19;1HA[19;80Ha[18;80HaB[19;80HB b[19;80HC"
"		c[19;2HC[19;80H[18;1HD[18;80Hd[19;1HE[19;80He"
"[18;80HeF[19;80HF f[19;80HG		g[19;2HG[19;80H"
"[18;1HH[18;80Hh[19;1HI[19;80Hi[18;80HiJ[19;80HJ j"
"[19;80HK		k[19;2HK[19;80H[18;1HL[18;80Hl[19;1HM"
"[19;80Hm[18;80HmN[19;80HN n[19;80HO		o[19;2HO"
"[19;80H[18;1HP[18;80Hp[19;1HQ[19;80Hq[18;80HqR[19;"
"80HR r[19;80HS		s[19;2HS[19;80H[18;1HT[18;80H"
"t[19;1HU[19;80Hu[18;80HuV[19;80HV v[19;80HW		w["
"19;2HW[19;80H[18;1HX[18;80Hx[19;1HY[19;80Hy[18;"
"80HyZ[19;80HZ z[?6l[r[22;1HPush <RETURN>"))))

(ert-deftest nterm-vttest-1-5 ()
  (should (nterm-ut 
"ef9d62ea5e6fda8be254102039b3255a" "a80541da236f0833e9c3ef725af7200d"
 (concat
"[?"
"3l[2J[1;1HTest of cursor-control characters inside ESC seq"
"uences.Below should be four identical lines:A B C D"
" E F G H IA[2CB[2CC[2CD[2CE[2CF[2CG[2CH[2"
"CI[2CA [2CB[4CC[6CD[8CE[10CF[12CG[14CH["
"16CI[20lA [1AB [1AC [1AD [1AE [1AF [1AG [1"
"AH [1AI [1APush <RETURN>"))))

(ert-deftest nterm-vttest-1-6 ()
  (should (nterm-ut 
"896bbb6c0159f94a75a1415ab62dedda" "ebe079c1fd8b81b5d61ea9188d17dc26"
 (concat
"[2J[1;1HTest of leading "
"zeros in ESC sequences.Two lines below you should see the"
" sentence \"This is a correct sentence\".[00000000004;0000000"
"01HT[00000000004;000000002Hh[00000000004;000000003Hi[0000"
"0000004;000000004Hs[00000000004;000000005H [00000000004;00"
"0000006Hi[00000000004;000000007Hs[00000000004;000000008H "
"[00000000004;000000009Ha[00000000004;0000000010H [00000000"
"004;0000000011Hc[00000000004;0000000012Ho[00000000004;0000"
"000013Hr[00000000004;0000000014Hr[00000000004;0000000015He"
"[00000000004;0000000016Hc[00000000004;0000000017Ht[000000"
"00004;0000000018H [00000000004;0000000019Hs[00000000004;00"
"00000020He[00000000004;0000000021Hn[00000000004;0000000022"
"Ht[00000000004;0000000023He[00000000004;0000000024Hn[0000"
"0000004;0000000025Hc[00000000004;0000000026He[20;1HPush <R"
"ETURN>"))))

;;; Test of screen features
(ert-deftest nterm-vttest-2-1 ()
  (should (nterm-ut 
"ad8d360cca3dd644390124a846e397cd" "33c2707fe415cd6bd36fbe99279982cb"
 (concat
"[2J[1;1H[?7h*******************************************"
"************************************************************"
"*********************************************************[?"
"7l[3;1H****************************************************"
"************************************************************"
"************************************************[?7h[5;1HT"
"his should be three identical lines of *'s completely fillin"
"gthe top of the screen without any empty lines between."
"(Test of WRAP AROUND mode setting.)Push <RETURN>"))))

(ert-deftest nterm-vttest-2-2 ()
  (should (nterm-ut 
"ec57a300b5c796408c7ae6d51ae2146a" "689c6d2d0443f7e27bf7f0d6ffacb1db"
 (concat
"[2J[3g"
"[1;1H[3CH[3CH[3CH[3CH[3CH[3CH[3CH[3CH[3CH"
"[3CH[3CH[3CH[3CH[3CH[3CH[3CH[3CH[3CH[3CH"
"[3CH[3CH[3CH[3CH[3CH[3CH[3CH[1;4H[0g[6C[0g"
"[6C[0g[6C[0g[6C[0g[6C[0g[6C[0g[6C[0g[6C[0g[6C"
"[0g[6C[0g[6C[0g[6C[0g[6C[1;7H[1g[2g[1;1H	*	*	*	*"
"	*	*	*	*	*	*	*	*	*[2;2H     *     *     *     *     *     *"
"     *     *     *     *     *     *     *[4;1HTest of TAB "
"setting/resetting. These two linesshould look the same. P"
"ush <RETURN>"))))

(ert-deftest nterm-vttest-2-3 ()
  (should (nterm-ut 
"0bd1cae765007548eae21bd37f206a02" "13255ee965350900167b32473909fe99"
 (concat
"[?5h[?3h[2J[1;1H[3g[8CH[8CH[8CH[8CH"
"[8CH[8CH[8CH[8CH[8CH[8CH[8CH[8CH[8CH[8CH"
"[8CH[8CH[8CH[1;1H123456789012345678901234567890123456"
"789012345678901234567890123456789012345678901234567890123456"
"78901234567890123456789012345678901[3;3HThis is 132 column "
"mode, light background.[4;4HThis is 132 column mode, light "
"background.[5;5HThis is 132 column mode, light background."
"[6;6HThis is 132 column mode, light background.[7;7HThis is"
" 132 column mode, light background.[8;8HThis is 132 column "
"mode, light background.[9;9HThis is 132 column mode, light "
"background.[10;10HThis is 132 column mode, light background"
".[11;11HThis is 132 column mode, light background.[12;12HT"
"his is 132 column mode, light background.[13;13HThis is 132"
" column mode, light background.[14;14HThis is 132 column mo"
"de, light background.[15;15HThis is 132 column mode, light "
"background.[16;16HThis is 132 column mode, light background"
".[17;17HThis is 132 column mode, light background.[18;18HT"
"his is 132 column mode, light background.[19;19HThis is 132"
" column mode, light background.[20;20HThis is 132 column mo"
"de, light background.Push <RETURN>"))))

(ert-deftest nterm-vttest-2-7 ()
  (should (nterm-ut 
"f70ca90ba5a60c526d2c66c0d9eac1c7" "d6f0b5ffd36573c384280a7e2ae7a63d"
 (concat
"[2J[?6h[?4h[12;13r[2J[24BSoft "
"scroll up region [12..13] size 2 Line 1Soft scroll up regi"
"on [12..13] size 2 Line 2Soft scroll up region [12..13] si"
"ze 2 Line 3Soft scroll up region [12..13] size 2 Line 4S"
"oft scroll up region [12..13] size 2 Line 5Soft scroll up "
"region [12..13] size 2 Line 6Soft scroll up region [12..13"
"] size 2 Line 7Soft scroll up region [12..13] size 2 Line "
"8Soft scroll up region [12..13] size 2 Line 9Soft scroll"
" up region [12..13] size 2 Line 10Soft scroll up region [1"
"2..13] size 2 Line 11Soft scroll up region [12..13] size 2"
" Line 12Soft scroll up region [12..13] size 2 Line 13Sof"
"t scroll up region [12..13] size 2 Line 14Soft scroll up r"
"egion [12..13] size 2 Line 15Soft scroll up region [12..13"
"] size 2 Line 16Soft scroll up region [12..13] size 2 Line"
" 17Soft scroll up region [12..13] size 2 Line 18Soft scr"
"oll up region [12..13] size 2 Line 19Soft scroll up region"
" [12..13] size 2 Line 20Soft scroll up region [12..13] siz"
"e 2 Line 21Soft scroll up region [12..13] size 2 Line 22"
"Soft scroll up region [12..13] size 2 Line 23Soft scroll u"
"p region [12..13] size 2 Line 24Soft scroll up region [12."
".13] size 2 Line 25Soft scroll up region [12..13] size 2 L"
"ine 26Soft scroll up region [12..13] size 2 Line 27Soft "
"scroll up region [12..13] size 2 Line 28Soft scroll up reg"
"ion [12..13] size 2 Line 29[24ASoft scroll down region [1"
"2..13] size 2 Line 1MMSoft scroll down region [12..13] s"
"ize 2 Line 2MMSoft scroll down region [12..13] size 2 Li"
"ne 3MMSoft scroll down region [12..13] size 2 Line 4M"
"MSoft scroll down region [12..13] size 2 Line 5MMSoft s"
"croll down region [12..13] size 2 Line 6MMSoft scroll do"
"wn region [12..13] size 2 Line 7MMSoft scroll down regio"
"n [12..13] size 2 Line 8MMSoft scroll down region [12..1"
"3] size 2 Line 9MMSoft scroll down region [12..13] size "
"2 Line 10MMSoft scroll down region [12..13] size 2 Line "
"11MMSoft scroll down region [12..13] size 2 Line 12M"
"MSoft scroll down region [12..13] size 2 Line 13MMSoft s"
"croll down region [12..13] size 2 Line 14MMSoft scroll d"
"own region [12..13] size 2 Line 15MMSoft scroll down reg"
"ion [12..13] size 2 Line 16MMSoft scroll down region [12"
"..13] size 2 Line 17MMSoft scroll down region [12..13] s"
"ize 2 Line 18MMSoft scroll down region [12..13] size 2 L"
"ine 19MMSoft scroll down region [12..13] size 2 Line 20"
"MMSoft scroll down region [12..13] size 2 Line 21MMSo"
"ft scroll down region [12..13] size 2 Line 22MMSoft scro"
"ll down region [12..13] size 2 Line 23MMSoft scroll down"
" region [12..13] size 2 Line 24MMSoft scroll down region"
" [12..13] size 2 Line 25MMSoft scroll down region [12..1"
"3] size 2 Line 26MMSoft scroll down region [12..13] size"
" 2 Line 27MMSoft scroll down region [12..13] size 2 Line"
" 28MMSoft scroll down region [12..13] size 2 Line 29M"
"MPush <RETURN>"))))

(ert-deftest nterm-vttest-2-8 ()
  (should (nterm-ut 
"57237d69771a6593eb105eb152bb0549" "fecd5cbe33f4903d12007abe3f80e063"
 (concat
"[1;24r[2J[24BSoft scroll up region [1..24] size 24 Line"
" 1Soft scroll up region [1..24] size 24 Line 2Soft scrol"
"l up region [1..24] size 24 Line 3Soft scroll up region [1"
"..24] size 24 Line 4Soft scroll up region [1..24] size 24 "
"Line 5Soft scroll up region [1..24] size 24 Line 6Soft s"
"croll up region [1..24] size 24 Line 7Soft scroll up regio"
"n [1..24] size 24 Line 8Soft scroll up region [1..24] size"
" 24 Line 9Soft scroll up region [1..24] size 24 Line 10S"
"oft scroll up region [1..24] size 24 Line 11Soft scroll up"
" region [1..24] size 24 Line 12Soft scroll up region [1..2"
"4] size 24 Line 13Soft scroll up region [1..24] size 24 Li"
"ne 14Soft scroll up region [1..24] size 24 Line 15Soft s"
"croll up region [1..24] size 24 Line 16Soft scroll up regi"
"on [1..24] size 24 Line 17Soft scroll up region [1..24] si"
"ze 24 Line 18Soft scroll up region [1..24] size 24 Line 19"
"Soft scroll up region [1..24] size 24 Line 20Soft scroll"
" up region [1..24] size 24 Line 21Soft scroll up region [1"
"..24] size 24 Line 22Soft scroll up region [1..24] size 24"
" Line 23Soft scroll up region [1..24] size 24 Line 24Sof"
"t scroll up region [1..24] size 24 Line 25Soft scroll up r"
"egion [1..24] size 24 Line 26Soft scroll up region [1..24]"
" size 24 Line 27Soft scroll up region [1..24] size 24 Line"
" 28Soft scroll up region [1..24] size 24 Line 29[24ASof"
"t scroll down region [1..24] size 24 Line 1MMSoft scroll"
" down region [1..24] size 24 Line 2MMSoft scroll down re"
"gion [1..24] size 24 Line 3MMSoft scroll down region [1."
".24] size 24 Line 4MMSoft scroll down region [1..24] siz"
"e 24 Line 5MMSoft scroll down region [1..24] size 24 Lin"
"e 6MMSoft scroll down region [1..24] size 24 Line 7M"
"MSoft scroll down region [1..24] size 24 Line 8MMSoft sc"
"roll down region [1..24] size 24 Line 9MMSoft scroll dow"
"n region [1..24] size 24 Line 10MMSoft scroll down regio"
"n [1..24] size 24 Line 11MMSoft scroll down region [1..2"
"4] size 24 Line 12MMSoft scroll down region [1..24] size"
" 24 Line 13MMSoft scroll down region [1..24] size 24 Lin"
"e 14MMSoft scroll down region [1..24] size 24 Line 15"
"MMSoft scroll down region [1..24] size 24 Line 16MMSoft"
" scroll down region [1..24] size 24 Line 17MMSoft scroll"
" down region [1..24] size 24 Line 18MMSoft scroll down r"
"egion [1..24] size 24 Line 19MMSoft scroll down region ["
"1..24] size 24 Line 20MMSoft scroll down region [1..24] "
"size 24 Line 21MMSoft scroll down region [1..24] size 24"
" Line 22MMSoft scroll down region [1..24] size 24 Line 2"
"3MMSoft scroll down region [1..24] size 24 Line 24MM"
"Soft scroll down region [1..24] size 24 Line 25MMSoft sc"
"roll down region [1..24] size 24 Line 26MMSoft scroll do"
"wn region [1..24] size 24 Line 27MMSoft scroll down regi"
"on [1..24] size 24 Line 28MMSoft scroll down region [1.."
"24] size 24 Line 29MMPush <RETURN>"))))

(ert-deftest nterm-vttest-2-9 ()
  (should (nterm-ut 
"ce90d4034120133556b3570be638ebd8" "db7c7631cc5ccfdcd51deb3ac0243dda"
 (concat
"[?4l[12;13r[2J[24B"
"Jump scroll up region [12..13] size 2 Line 1Jump scroll up"
" region [12..13] size 2 Line 2Jump scroll up region [12..1"
"3] size 2 Line 3Jump scroll up region [12..13] size 2 Line"
" 4Jump scroll up region [12..13] size 2 Line 5Jump scrol"
"l up region [12..13] size 2 Line 6Jump scroll up region [1"
"2..13] size 2 Line 7Jump scroll up region [12..13] size 2 "
"Line 8Jump scroll up region [12..13] size 2 Line 9Jump s"
"croll up region [12..13] size 2 Line 10Jump scroll up regi"
"on [12..13] size 2 Line 11Jump scroll up region [12..13] s"
"ize 2 Line 12Jump scroll up region [12..13] size 2 Line 13"
"Jump scroll up region [12..13] size 2 Line 14Jump scroll"
" up region [12..13] size 2 Line 15Jump scroll up region [1"
"2..13] size 2 Line 16Jump scroll up region [12..13] size 2"
" Line 17Jump scroll up region [12..13] size 2 Line 18Jum"
"p scroll up region [12..13] size 2 Line 19Jump scroll up r"
"egion [12..13] size 2 Line 20Jump scroll up region [12..13"
"] size 2 Line 21Jump scroll up region [12..13] size 2 Line"
" 22Jump scroll up region [12..13] size 2 Line 23Jump scr"
"oll up region [12..13] size 2 Line 24Jump scroll up region"
" [12..13] size 2 Line 25Jump scroll up region [12..13] siz"
"e 2 Line 26Jump scroll up region [12..13] size 2 Line 27"
"Jump scroll up region [12..13] size 2 Line 28Jump scroll u"
"p region [12..13] size 2 Line 29[24AJump scroll down regi"
"on [12..13] size 2 Line 1MMJump scroll down region [12.."
"13] size 2 Line 2MMJump scroll down region [12..13] size"
" 2 Line 3MMJump scroll down region [12..13] size 2 Line "
"4MMJump scroll down region [12..13] size 2 Line 5MMJ"
"ump scroll down region [12..13] size 2 Line 6MMJump scro"
"ll down region [12..13] size 2 Line 7MMJump scroll down "
"region [12..13] size 2 Line 8MMJump scroll down region ["
"12..13] size 2 Line 9MMJump scroll down region [12..13] "
"size 2 Line 10MMJump scroll down region [12..13] size 2 "
"Line 11MMJump scroll down region [12..13] size 2 Line 12"
"MMJump scroll down region [12..13] size 2 Line 13MMJ"
"ump scroll down region [12..13] size 2 Line 14MMJump scr"
"oll down region [12..13] size 2 Line 15MMJump scroll dow"
"n region [12..13] size 2 Line 16MMJump scroll down regio"
"n [12..13] size 2 Line 17MMJump scroll down region [12.."
"13] size 2 Line 18MMJump scroll down region [12..13] siz"
"e 2 Line 19MMJump scroll down region [12..13] size 2 Lin"
"e 20MMJump scroll down region [12..13] size 2 Line 21"
"MMJump scroll down region [12..13] size 2 Line 22MMJump"
" scroll down region [12..13] size 2 Line 23MMJump scroll"
" down region [12..13] size 2 Line 24MMJump scroll down r"
"egion [12..13] size 2 Line 25MMJump scroll down region ["
"12..13] size 2 Line 26MMJump scroll down region [12..13]"
" size 2 Line 27MMJump scroll down region [12..13] size 2"
" Line 28MMJump scroll down region [12..13] size 2 Line 2"
"9MMPush <RETURN>"))))

(ert-deftest nterm-vttest-2-8 ()
  (should (nterm-ut 
"f6c6e5734a13c0d8a8443188d1467c7c" "2c7729fcec5c92cb915ba7456e41ed1e"
 (concat
"[1;24r[2J[24BJump scroll up region [1"
"..24] size 24 Line 1Jump scroll up region [1..24] size 24 "
"Line 2Jump scroll up region [1..24] size 24 Line 3Jump s"
"croll up region [1..24] size 24 Line 4Jump scroll up regio"
"n [1..24] size 24 Line 5Jump scroll up region [1..24] size"
" 24 Line 6Jump scroll up region [1..24] size 24 Line 7Ju"
"mp scroll up region [1..24] size 24 Line 8Jump scroll up r"
"egion [1..24] size 24 Line 9Jump scroll up region [1..24] "
"size 24 Line 10Jump scroll up region [1..24] size 24 Line "
"11Jump scroll up region [1..24] size 24 Line 12Jump scro"
"ll up region [1..24] size 24 Line 13Jump scroll up region "
"[1..24] size 24 Line 14Jump scroll up region [1..24] size "
"24 Line 15Jump scroll up region [1..24] size 24 Line 16J"
"ump scroll up region [1..24] size 24 Line 17Jump scroll up"
" region [1..24] size 24 Line 18Jump scroll up region [1..2"
"4] size 24 Line 19Jump scroll up region [1..24] size 24 Li"
"ne 20Jump scroll up region [1..24] size 24 Line 21Jump s"
"croll up region [1..24] size 24 Line 22Jump scroll up regi"
"on [1..24] size 24 Line 23Jump scroll up region [1..24] si"
"ze 24 Line 24Jump scroll up region [1..24] size 24 Line 25"
"Jump scroll up region [1..24] size 24 Line 26Jump scroll"
" up region [1..24] size 24 Line 27Jump scroll up region [1"
"..24] size 24 Line 28Jump scroll up region [1..24] size 24"
" Line 29[24AJump scroll down region [1..24] size 24 Line "
"1MMJump scroll down region [1..24] size 24 Line 2MMJ"
"ump scroll down region [1..24] size 24 Line 3MMJump scro"
"ll down region [1..24] size 24 Line 4MMJump scroll down "
"region [1..24] size 24 Line 5MMJump scroll down region ["
"1..24] size 24 Line 6MMJump scroll down region [1..24] s"
"ize 24 Line 7MMJump scroll down region [1..24] size 24 L"
"ine 8MMJump scroll down region [1..24] size 24 Line 9"
"MMJump scroll down region [1..24] size 24 Line 10MMJump"
" scroll down region [1..24] size 24 Line 11MMJump scroll"
" down region [1..24] size 24 Line 12MMJump scroll down r"
"egion [1..24] size 24 Line 13MMJump scroll down region ["
"1..24] size 24 Line 14MMJump scroll down region [1..24] "
"size 24 Line 15MMJump scroll down region [1..24] size 24"
" Line 16MMJump scroll down region [1..24] size 24 Line 1"
"7MMJump scroll down region [1..24] size 24 Line 18MM"
"Jump scroll down region [1..24] size 24 Line 19MMJump sc"
"roll down region [1..24] size 24 Line 20MMJump scroll do"
"wn region [1..24] size 24 Line 21MMJump scroll down regi"
"on [1..24] size 24 Line 22MMJump scroll down region [1.."
"24] size 24 Line 23MMJump scroll down region [1..24] siz"
"e 24 Line 24MMJump scroll down region [1..24] size 24 Li"
"ne 25MMJump scroll down region [1..24] size 24 Line 26"
"MMJump scroll down region [1..24] size 24 Line 27MMJum"
"p scroll down region [1..24] size 24 Line 28MMJump scrol"
"l down region [1..24] size 24 Line 29MMPush <RETURN>"))))

(ert-deftest nterm-vttest-2-9 ()
  (should (nterm-ut
"be9e1cda05e462b8741b1e170421d8eb" "4e73d78f3c4e0914862fcbb1f14d3bb8"
 (concat
"[?6h[2J"
"[23;24rOrigin mode test. This line should be at the botto"
"m of the screen.[1;1HThis line should be the one above the "
"bottom of the screen. Push <RETURN>"))))

(ert-deftest nterm-vttest-2-11 ()
  (should (nterm-ut 
"717f7f37fdc1b05bdb2d89c1ff402bd6" "b82f146cded7b046ed104d3f79191d80"
 (concat
"[2J[?6l[24;1HOrigin mo"
"de test. This line should be at the bottom of the screen.[1"
";1HThis line should be at the top of the screen. Push <RETUR"
"N>"))))

(ert-deftest nterm-vttest-2-12 ()
  (should (nterm-ut
"417ec59bc343d9e5c81cc0b78b559290" "2f79b4f07884791de4d68411bb7b88c8"
 (concat
"[1;24r[2J[1;20HGraphic rendition test pattern:[4;1H[0"
"mvanilla[4;40H[0;1mbold[6;6H[;4munderline[6;45H[;1m[4"
"mbold underline[8;1H[0;5mblink[8;40H[0;5;1mbold blink[1"
"0;6H[0;4;5munderline blink[10;45H[0;1;4;5mbold underline "
"blink[12;1H[1;4;5;0;7mnegative[12;40H[0;1;7mbold negativ"
"e[14;6H[0;4;7munderline negative[14;45H[0;1;4;7mbold und"
"erline negative[16;1H[1;4;;5;7mblink negative[16;40H[0;1"
";5;7mbold blink negative[18;6H[0;4;5;7munderline blink neg"
"ative[18;45H[0;1;4;5;7mbold underline blink negative[m[?"
"5l[23;1H[0KDark background. Push <RETURN>"))))

(ert-deftest nterm-vttest-2-14 ()
  (should (nterm-ut
"def090a2fe1a7a51c953ce8dd4a10343" "2f37168f73de511effd3f6d40b7b0e63"
 (concat
"[?5l[2J[8;12Hnormal[8;24Hb"
"old[8;36Hunderscored[8;48Hblinking[8;60Hreversed[10;1Hst"
"ars:[12;1Hline:[14;1Hx'es:[16;1Hdiamonds:[10;12H[;0m(B"
")B*****7[1;1H[m(B)BA8*****[10;24H[;1m(B)B*****"
"7[1;2H[m(B)BA8*****[10;36H[;4m(B)B*****7[1;3H"
"[m(B)BA8*****[10;48H[;5m(B)B*****7[1;4H[m(B)B"
"A8*****[10;60H[;7m(B)B*****7[1;5H[m(B)BA8*****"
"[12;12H[;0m(0)Bqqqqq7[2;1H[m(B)BA8qqqqq[12;24H["
";1m(0)Bqqqqq7[2;2H[m(B)BA8qqqqq[12;36H[;4m(0)B"
"qqqqq7[2;3H[m(B)BA8qqqqq[12;48H[;5m(0)Bqqqqq7"
"[2;4H[m(B)BA8qqqqq[12;60H[;7m(0)Bqqqqq7[2;5H[m"
"(B)BA8qqqqq[14;12H[;0m(B)Bxxxxx7[3;1H[m(B)BA8"
"xxxxx[14;24H[;1m(B)Bxxxxx7[3;2H[m(B)BA8xxxxx[14"
";36H[;4m(B)Bxxxxx7[3;3H[m(B)BA8xxxxx[14;48H[;5m"
"(B)Bxxxxx7[3;4H[m(B)BA8xxxxx[14;60H[;7m(B)Bxx"
"xxx7[3;5H[m(B)BA8xxxxx[16;12H[;0m(0)B`````7[4;"
"1H[m(B)BA8`````[16;24H[;1m(0)B`````7[4;2H[m(B"
")BA8`````[16;36H[;4m(0)B`````7[4;3H[m(B)BA8```"
"``[16;48H[;5m(0)B`````7[4;4H[m(B)BA8`````[16;60"
"H[;7m(0)B`````7[4;5H[m(B)BA8`````[0m(B)B[21;"
"1HTest of the SAVE/RESTORE CURSOR feature. There shouldbe"
" ten characters of each flavour, and a rectangleof 5 x 4 "
"A's filling the top left of the screen.Push <RETURN>"))))

(ert-deftest nterm-vt100-ed-1 ()
  "Test VT100 ED partial delete."
  (should (nterm-ut
"200e3d98bfff785a3ae3c34b3e51bf8e" "3a6c01ada23aca922212b949e7beee53"
  (concat
"#8"
"[10;20H1[1J"
"[20;30H1[0J"))))

(ert-deftest nterm-vt100-ed-2 ()
  "Test VT100 ED complete delete.
Make sure the double width line gets resetted."
  (nterm-ut
"16cc13d28218738d5d8688f7a4e0e4e5" "971a0e38dce04387a30cf29bfddfac4a"
"#8#6[2J"))

(ert-deftest nterm-vt100-ed-3 ()
  "Test VT100 ED partial delete on double width line"
  (nterm-ut
"9f535039ec87637af83c5812ce282af5" "3e4391164b1188c2ac007e49bf90d01d"
 (concat
"#8#6"
"[10;20H#61[1J"
"[20;30H1#6[0J")))

(ert-deftest nterm-vt100-el ()
  "Test VT100 EL.
Double line attribute should not be cleared. Test partial erase
with argument 0 and 1."
  (should (nterm-ut
"3cc5918951990ccfc54e74a837310b2a" "dda2b65811ffb887c48153cec7a2282c"
 (concat
"1234[0K"
"1234[1K"
"1234[2K"
"1234[3K"
"#61234[0K"
"#61234[1K"
"#61234[2K"))))

;; blink dwl breakage
;; [5m
;; [1;1H
;; [2J
;; #3T

;; Copyright (C) 2010 Ivan Kanis
;; Author: Ivan Kanis
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
