        Emacs-wget USAGE  ($Date: 2004/10/16 08:06:40 $)

Emacs-wget の使用法について解説します。

1. wget コマンド

   Emacs-wget から使えるコマンドを説明します。

   なお、ダウンロードしたファイルは変数 wget-download-directory で指定し
   たディレクトリーに保存されます。デフォールト値は ~/download です。本ド
   キュメントでは、ファイルのダウンロード先を『ダウンロード・ディレクト
   リー』と呼びます。

1.1. M-x wget

     基本となるダウンロード用のコマンドです。

     ファイル名を尋ねてくるので、ダウンロードしたいファイルの URL を入力
     します。

     C-u M-x wget で、ダウンロード・ディレクトリーや wget に渡すオプショ
     ンを一時的に変更できます。

     wget に渡すオプションは long option 形式 [1] で補完が効きます。補完
     には TAB キーを用います。オプションの区切りには SPACE を使います。

     古い wget では long option をサポートしていない場合があります。お使
     いの wget のバージョンを確かめて下さい。また、補完リストは wget
     v.1.9.1 の Info をもとに作成しました。古い wget では使えないオプショ
     ンもあります。

     [1] long option 形式: 二つのハイフン `--' が最初に付くオプションの形式。

1.2. M-x wget-web-page

     対象ファイルと、対象ファイルからリンクしているページをダウンロード
     します。

     C-u M-x wget-web-page でダウンロード・ディレクトリーと wget に渡すオ
     プションを一時的に変更できます。

1.3. M-x wget-quit

     wget を終了させます。複数のダウンロード・プロセスがある場合、終了さ
     せるプロセスの候補を表示します。

     wget の終了は *wget* バッファを使う方が簡単です。


2. *wget* バッファ

   ダウンロードを始めると、ウィンドウが二つに分かれて次のようなバッファが
   出来ます。これを *wget* バッファと呼びます。*wget* バッファは、ダウン
   ロードの進行状況を表示します。

      ==================================================================
        -- Wget Process ---
        [ 24%]**         http://www.foo.org/emacs-wget/wget-buffer.txt
        =*=DOWNLOADED=*= ~/download/bar.jpg
      ==== *wget* ======== (wget) ======================================

   *wget* バッファでは wget の終了などをする事が出来ます。*wget* バッファ
   内でのキー・バインドは次の通りです。

     キー    操作
     ----   ------
      d      ポイントのある行のダウンロードを中止します。
      q      *wget* バッファを隠します。
      Q      全てのダウンロードを中止して、*wget* バッファを消します。
      g      *wget* バッファの再描画。
      i	     wget プロセスの情報を更新。
      n      次の wget プロセスへ。
      p      前の wget プロセスへ。
     ----   ------

   wget プロセスの情報は、自動更新されません。更新させたい時は `i' を押
   して下さい。


3. ダウンロード・ディレクトリー・フィルター

   emacs-wget では、ダウンロード・ディレクトリーをより手軽に変更する仕組
   を用意しています。例えば、ファイルの拡張子からダウンロード・ディレクト
   リーを変えたり、ダウンロード・ディレクトリーにエイリアス (別名) をつけ
   たり、または、その両方を組み合わせたりできます。

   この仕組を使うには、変数 wget-download-directory-filter と
   wget-download-directory の設定が必要です。
   wget-download-directory-filter には、ダウンロード・ディレクトリーを変
   更するための関数 (フィルター関数) を指定します。そして、その関数に合う
   ように wget-download-directory を書きかえます。

3.1. フィルター関数を指定する

3.1.1. REGEXP でダウンロード・ディレクトリーを振り分ける

     (setq wget-download-directory-filter #'wget-download-dir-filter-regexp)

     変数 wget-download-directory は、(REGEXP . DIR) な alist にします。
     REGEXP が URL にマッチしたら、対応する DIR をダウンロード・ディレク
     トリーにします。マッチする REGEXP がなければ、ダウンロード・ディレク
     トリーを訊きます。

     例:
     (setq wget-download-directory
          '(("\\.\\(jpe?g\\|png\\)$" . "~/pictures")
            ("\\.el$" . "~/site-lisp")
            ("." . "~/download")))

     上の例では、jpeg, jpg, png の拡張子を持ったファイルは ~/pictures に、
     Emacs-Lisp ファイル (*.el) は ~/site-lisp に、それ以外のファイルは
     ~/download に保存します。

3.1.2. ダウンロード・ディレクトリーにエイリアスをつける

     (setq wget-download-directory-filter #'wget-download-dir-filter-alias)

     変数 wget-download-directory は、(ALIAS . DIR) な alist にします。

     M-x wget すると、ダウンロード・ディレクトリーの alias を訊きます。
     ちなみに、予め用意した alias ディレクトリー以外をダウンロード・ディ
     レクトリーには選べません。

     C-u M-x wget とすると、alias を訊いた後もう一度 alias をもとにしてダ
     ウンロード・ディレクトリーを訊きます。

     もし、"default" という alias があれば、M-x wget では alias を訊かず、
     "default" に対応するディレクトリーにファイルを保存します。

     例:
     (setq wget-download-directory
          '(("pics" . "~/pictures")
            ("elisp" . "~/site-lisp")
            ("default" . "~/download")))

     上の例で M-x wget すると、ファイルは ~/download へ保存されます。もし
     "default" alias がなければ、alias を訊ねるプロンプトが現れて pics か
     elisp を選ぶ事ができます。

     C-u M-x wget すると、alias を訊いてきます。例えば、ファイルを
     ~/pictures/dogs というディレクトリーに保存したい場合を考えてみましょ
     う。まず、"pics" alias を選びます。すると以下のようなプロンプトが現
     れます:

       Download directory: ~/pictuers/-!-

     そこで ~/pictures/dogs と入力します。


3.1.3. REGEXP で振り分けた後、エイリアスを訊ねる

     (setq wget-download-directory-filter #'wget-download-dir-filter-regexp-and-alias)

     wget-download-directory-filter-regexp の後に
     wget-download-directory-filter-alias を呼んでいるだけです。従って、
     変数 wget-download-directory も 3.1.1. と 3.1.2. の組み合わせになり
     ます。つまり、(REGEXP . DIR) か
     (REGEXP . ((ALIAS . DIR) (ALIAS .  DIR)... )) という組合わせです。

     例:
     (setq wget-download-directory
          '(("\\.\\(jpe?g\\|png\\)$" .
              (("dog" . "~/dogs/picture")
               ("cat" . "~/cats/picture")
               ("default" . "~/pictures")))
            ("\\.el$" . "~/site-lisp")
            ("." . "~/download"))

     まず、*.el なファイルは ~/site-lisp に保存されます。これは
     wget-download-directory-filter-regexp の場合と同じです。

     *.jpeg, *.jpg, *.png なファイルの場合は少し複雑です。M-x wget した場
     合、"default" alias があるのでファイルは ~/pictures に保存されます。

     C-u M-x wget した場合、alias を訊いてきます。ここで alias の候補は
     dog, cat, default の三つです。alias を選ぶと、もう一度ダウンロード・
     ディレクトリーを訊いてきます。ここら辺は
     wget-download-directory-filter-alias の場合と同じです。

     残りのファイルは ~/download に保存されます。

3.1.4. カレント・ディレクトリーがダウンロード・ディレクトリー下にあるか調べる。

     (setq wget-download-directory-filter #'wget-download-dir-filter-current-dir)

     このフィルター関数の場合、変数 wget-download-directory は変更しません。

     もし、カレント・ディレクトリーが wget-download-directory 以下であっ
     た場合、カレント・ディレクトリーにファイルを保存します。 カレント・
     ディレクトリーが wget-download-directory 以下でないならば、ダウンロー
     ド・ディレクトリーを訊きます。


3.2. フィルター関数を自作する

     フィルター関数は三つの引数を取り、ダウンロード・ディレクトリーを返すよ
     うに作ります。フィルター関数の雛型は次の通りです。

     (defun my-wget-download-dir-filter (arg uri dir)
        body...
     )

     第一引数 ARG は、C-u が押されたかどうかです。C-u 付で呼ばれた場合、
     `t' が入ります。

     第二引数 URI は、ダウンロードするファイルの URI です。

     第三引数 DIR は、ダウンロード・ディレクトリーです。
     wget-download-directory の値が入ります。

     返し値は、ダウンロード・ディレクトリーを表す文字列です。


4. ダウンロード・ログ

   emacs-wget には、ダウンロードしたファイルのログを取る機能があります。
   ログを取るには、変数 wget-download-log-file にログ・ファイルを指定しま
   す。

   ex.
   (setq wget-download-log-file "log.txt")

   ログのフォーマットは、二つの変数 wget-download-log-format と
   wget-download-log-time-format で決められます。デフォールトでは、次のよ
   うなログが取れます。

     2004-01-01 01:02:34	http://www.foo.org/bar.txt

   ログは、ログ・ファイルの末尾に追加されます。ファイルの先頭にログを追加
   するには、変数 wget-add-download-log-eof を nil にします。

   もしかしたら、全てのダウンロード・ディレクトリーでログ・ファイルを作り
   たくない人もいるでしょう。デフォールトでは、C-u M-x wget で一時的にダ
   ウンロード・ディレクトリーを切り替えた場合でも、ログ・ファイルを作ろう
   とします。この挙動を変えるには、変数 wget-download-create-log の値を変
   えます。nil にすると、ログ・ファイルを新たに作りません この場合ログを
   取るには、予め touch コマンドか何かで空のファイルを作っておく必要があ
   ります。'ask にすると、ログ・ファイルの有無に応じてログ・ファイルを作
   るかどうか訊いてきます。デフォールト値は 'always で、ログ・ファイルが
   なければ作ります。


5. Emacs-w3m (Emacs/W3) との連携

   Emacs-w3m (Emacs/W3) は Emacs 上の Web ブラウザーです。

   Emacs-w3m:
     http://emacs-w3m.namazu.org/

   Emacs/W3:
     http://www.cs.indiana.edu/elisp/w3/docs.html

   README.ja の c-1.) にある設定をすると Emacs-w3m のダウンロード用のキー
   を emacs-wget で上書きします。

   例えば、Emacs-w3m で `d' にダウンロード用のコマンドがバインドされてい
   た場合、リンクの上にポイントを持って行き `d' を押すと、wget を使ってリ
   ンク先ファイルのダウンロードを始めます。

   また、リンクの無い所で `d' を押すと、現在訪れているページのダウンロー
   ドを開始します。


6. カスタマイズ

   カスタマイズ用の変数を解説します。変数名の次行は、デフォールトの変数
   値です。

6.1. wget-command
     "wget"

     外部で呼び出す wget コマンドの名前です。
     PATH が通っていない場合、絶対パスで指定して下さい。

6.2. download directory

6.2.1. wget-download-directory
       "~/download"

     Emacs-wget のダウンロード・ディレクトリーです。

     `nil' なら、ダウンロード・ディレクトリーを毎回訊ねます。

     変数 wget-download-directory-filter を設定した場合、3.1. 節を参考に
     wget-download-directory の値を直して下さい。

6.2.2. wget-download-directory-filter
       nil

     ダウンロード・ディレクトリーを動的に変えるための関数を指定します。

     現在、4 つの関数があります。

       wget-download-directory-filter-regexp
         regexp でダウンロード・ディレクトリーを振り分ける。
       wget-download-directory-filter-alias
         ダウンロード・ディレクトリーに alias をつける。
       wget-download-directory-filter-regexp-and-alias
         regexp で振り分けた後、alias をたずねる。
       wget-download-directory-filter-current-dir
         カレント・ディレクトリーがダウンロード・ディレクトリー下にあるか調べる。

     この変数を指定した場合、指定した関数に応じて 変数
     wget-download-directory を変更する必要があります。詳しくは 3.1. をお
     読み下さい。

6.3. wget options

6.3.1. wget-default-options
       nil

     Emacs-wget が使うデフォールトのオプションです。

     wget のオプションについては GNU wget の info を参照して下さい。

6.3.2. wget-web-page-options
       '("-r" "-L" "-np")

     web ページ全部をダウンロードする時に wget に渡すオプション引数です。
     wget-web-page-options が明示的に指定されない場合、
     wget-default-options の値を引き継ぎます。

     wget のオプションについては GNU wget の info を参照して下さい。

6.3.3. wget-ftp-default-options
       nil

     FTP からファイルをダウンロードする時に使うオプション引数です。

     nil なら wget-default-options の値を代わりに使います。

     wget-default-options に追加する形でオプションを設定したい場合は、
     次のようにします。

       (add-hook 'wget-load-hook
                 (lambda ()
	           (setq wget-ftp-default-options
                         (append wget-default-options '("--passive-ftp")))))

     上の例では、FTP からのダウンロードは --passive-ftp を使うようにな
     ります。

6.4. wget-executable-file-extension-list
     nil

     ダウンロード後、実効権限を与えるファイルの拡張子のリストです。

     例えば、Shell スクリプトや Perl スクリプト、Ruby スクリプトをダウン
     ロードした後に、実効権限を与えるには、次のように設定します。

     (setq wget-executable-file-extension-list '("sh" "csh" "pl" "rb"))

6.5. log file

6.5.1. wget-download-log-file
       nil

     ダウンロード・ログを取るファイルの名前です。

     nil なら、ログを取りません。

6.5.2. wget-download-create-log
       'always

     ログ・ファイルがない場合の挙動を決めます。

       'always	ログ・ファイルを作ります
       'ask	ログ・ファイルを作るか訊ねます
       nil	ログ・ファイルを作りません。

     nil の場合、ログを取るには空のログ・ファイルを予め作っておきます。

6.5.3. wget-download-log-format
       "%T\t%U\n"

     ログのフォーマットを決めます。

       %T	wget-download-log-time-format の値で置換されます
       %t	タイトルの入力を促します
       %U	ファイルの URI で置換されます。

     `\t' はタブ文字に、`\n' は改行になります。

6.5.4. wget-download-log-time-format
       "%Y-%m-%d %H:%M:%S"

     ダウンロードを開始した時間のフォーマットです。
     wget-download-log-format の %T 部分を置き換えます。

     関数 format-time-string で使える表現が使用可能です。

6.5.5. wget-add-download-log-eof
       t

     ログを追記する場所を指定します。

     nil ならログ・ファイルの先頭に、t ならログ・ファイルの末尾にログを追
     記します。

6.6. hooks

6.6.1. wget-hook
       nil

     wget 関連の関数を呼び出した後に呼ばれる Hook です。

6.6.2  wget-after-hook
       nil

     ファイルをダウンロードした後に呼ばれる Hook です。

6.6.3. wget-load-hook
       nil

     wget.el をロードする時に実行する Hook です。

6.7. wget-process-buffer
     "*wget*"

     nil を設定すると *wget* バッファを表示しません。

   主要な変数は以上です。その他のオプションを変更したければ、M-x
   edit-options か M-x customize して下さい。


7. TIPS

7.1. カレント・ディレクトリーにファイルをダウンロードする。

      (setq wget-download-directory "./")


8. USAGE.ja における謝辞

   新谷明彦 <shinya@sonicteam.com> さんにはドキュメントの誤りを指摘して頂
   きました。



Local Variables:
mode: indented-text
coding: shift_jis-dos
fill-column: 72
End:
