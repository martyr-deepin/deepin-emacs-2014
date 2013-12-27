        Emacs-wget README  ($Date: 2004/10/14 19:58:07 $)

このパッケージには wget を Emacs 上で動かすためのプログラムが含まれてい
ます。


1. はじめに

   GNU wget は HTTP/FTP に対応した非対話的なダウンロード・ツールです。
   Hrvoje Niksic さんによって開発され、フリーウェアとして公開されていま
   す。詳細は以下のサイトを参照して下さい。

   http://www.gnu.org/software/wget/


   emacs-wget は、Emacs 上で動作する wget のインターフェース・プログラム
   です。Emacs 上で動作する Web ブラウザー emacs-w3/w3m と一緒に使う事も
   出来ます。

   最新の emacs-wget は、以下のサイトで公開しています。

   http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget/


2. 必要なもの

   GNU Emacs と GNU wget が必要です。

   Emacs: http://www.gnu.org/software/emacs/
   Wget:  http://www.gnu.org/software/wget/


3. インストール

   a.) UNIX であれば、以下の手順でインストールできます。

        $ tar xzvf emacs-wget-X.YY.tar.gz
        $ cd emacs-wget-X.YY
        $ make
        # make install

   a-1.) make に失敗するようでしたら、手作業でインストールして下さい。

      同梱されている Emacs-Lisp ファイルをパスの通っている場所にコピー 
      (若しくは移動) するだけです。バイト・コンパイルはお好みでどうぞ。


   b.) .emacs に次の記述を加えます。

       	(autoload 'wget "wget" "wget interface for Emacs." t)
       	(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)


   c.) Emacs 上の Web ブラウザーと一緒に使う場合:

   c-1.) Emacs-w3m と一緒に使う場合:

       次のコードを ~/.emacs に加えます。

        (load "w3m-wget")

   c-2.) Emacs-w3 と一緒に使う場合:

       次のコードを ~/.emacs に加えます。

        (autoload 'w3-wget "w3-wget" "wget interface for Emacs/W3." t)


   d) wget のバージョンが 1.7 以下の場合:

       次のコードを .emacs に追加して下さい。

        (setq wget-basic-options '("-v"))


   e) .wgetrc に以下の設定をしている場合:

   e-1.) quiet = on

         Emacs-wget でダウンロードに失敗します。次の設定を加えて下さい。

	(setq wget-basic-options (cons "-equiet=off" wget-basic-options))

   e-2.) dir_prefix = PATH/TO/DOWNLOAD

         Emacs-wget のダウンロード先指定が無視されます。次の設定を加えて
         下さい。

	(setq wget-basic-options (cons "-P." wget-basic-options))

   e-3.) timestamping = on
         mirror = on  (mirror=on は timestamping=on を自動的に設定します)

	 -nc オプションと競合します。wget-default-options と 
	 wget-ftp-default-options に "-nc" が入っていない事を確認して下
	 さい。

   e-4.) logfile = filename

         *wget* バッファは動作しません。.wgetrc から上記設定を削除するか、
         変数 wget-process-buffer を nil にセットして下さい。

	 (setq wget-process-buffer nil)


   f) wget に PATH が通っていない場合、wget-command を設定します。
      Meadow をアイコンから起動した時、PATH が設定されていないかもしれま
      せん。

      例えば C:\cygwin\bin\wget.exe に wget があるようなら、次のように変
      更します。

        (setq wget-command "C:/cygwin/bin/wget")

      パスは、あなたの環境に合わせて適宜変えて下さい。


4. 動作確認

   動作確認は

   emacs-version:  20.7 21.3.50
   wget -V:        1.7, 1.8.1, 1.8.2, 1.9.1

   でしました。


5. 連絡先

   メールを <ataka@milk.freemail.ne.jp> 宛に送ってください。
   不具合があった場合、使っている wget のバージョンも教えて下さい。

   上記以外の環境で動作確認をされた方がいらっしゃいましたら、是非安宅宛に
   メールを送って下さい。特に 古いバージョンの Emacs で、動作確認が取れ
   ていないので、お知らせを頂けると嬉しいです。

   また、Emacs/W3 と一緒に使っている方、いらっしゃいましたら情報をお寄せ
   下さい。


6. 謝辞

   GNU wget がなければ、このプログラムは存在しませんでした。最大の謝意を、
   Hrvoje Niksic さんに捧げます。

   荒川隆行氏に感謝の意を捧げます。彼の言葉と協力とテストが無かったら、
   このソフトは開発にもっと時間がかかった事でしょう。

   Juri Linkov さんには、emacs-wget 0.5.0 のブラッシュ・アップを手伝って
   もらいました。ここには書ききれない位いの提案とバグ・レポートを頂き、
   度重なるテストにお付き合いしてもらいました。

   中山さんは、emacs-wget を FreeBSD Ports Collection に含める作業をして
   下さっています。

   田郷明さんは、emacs-wget の Debian Package を作って下さっています。


   坂本貢さん
		Meadow pre1.15 での動作確認
   鶴田尚樹さん
		Meadow 1.15 での動作確認と cygwin 版 wget 1.8.2 挙動について
   小関吉則さん
		w3m-wget.el のロード方法について
   新谷明彦さん
		.wgetrc で設定をしていた場合の挙動について
		(quiet, timestamping, mirror, dir_prefix, logfile).
   中谷俊晴さん
		- Emacs-wget が LANG 環境変数を Emacs のシステム変数から
                消除してしまうバグの報告とその解決方法の教示
		- 関数 read-directory-name が Emacs-21 以降で導入された
                関数である事の報告
   橘和さん
                ダウンロード後、パーミッションの変更について
   mice さん
                wget-download-directory が nil の時の挙動について


   動作確認、バグ・レポート etc... でお世話になりました。
   ありがとうございます。


Local Variables:
mode: indented-text
coding: shift_jis-dos
fill-column: 72
End:
