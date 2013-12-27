1> Deepin Emacs 的由来
本人作为Emacs玩家, 开发了无数Emacs扩展: http://www.emacswiki.org/emacs/AndyStewart
为了广大的深度战友, 故开发维护 Deepin Emacs.

2> Deepin Emacs 包括什么?
编辑器, 调试器, 浏览器, 音乐播放器, 文件管理器, 终端, IRC客户端, 邮件客户端, 新闻阅读器, 电子书阅读器, Debian包管理器, 英文翻译助手, 语音助手, Emacs安装器, 代码补全, 代数计算系统, 日历, GTD客户端, 电驴客户端, 下载管理器, 进程管理器, 图片浏览器, Wiki编辑器, 粘贴客户端, Vi模式, 正则表达式反馈助手, 自动补全, 代码索引, 实时语法检查, 代码画图, Anything超强搜索, Man/Woman, 键盘宏, 代码锁屏, 自动记忆session ... 只有你想不到的, 没有Emacs做不到的

3> Deepin Emacs 快捷安装:
Linux Deepin 用户: 
sudo apt-get install deepin-emacs -y

键入 deepin-emacs 就可以使用

4> Deepin Emacs 编译安装:

* 目录说明:
emacs-24.0.94.1_xwidget/           Emacs 24.0.94.1 源代码 (编译用, 包括xwidget支持, 可以直接在Emacs中打开Webkit浏览器)

lazycat-lisp/
 |
 |--- Site-Lisp
      |
      |--- Configure/              LazyCat 自己的配置文件
  
lazycat-lisp/
 |
 |--- Site-Lisp
      |
      |--- Packages/               LazyCat 收集和开发的 Emacs 扩展
  
lazycat-lisp/
 |
 |--- Configure-File               Deepin Emacs 初始化配置文件, 用户自己的配置文件会在 ~/.emacs.d/deepin-emacs/Configure-File 下保存
 
lazycat-lisp/
 |
 |--- Document                     一些文档文件
 
lazycat-lisp/
 |
 |--- Gnus                         Gnus 数据文件
 
lazycat-lisp/
 |
 |--- Mail                         Gnus 数据文件
 
lazycat-lisp/
 |
 |--- Image                        一些图片
 
.emacs                             Deepin Emacs 配置文件, 放置在 HOME 目录下

* 安装Emacs编译依赖: 
sudo apt-getinstall libxaw7-dev libxpm-dev libpng12-dev libjpeg-dev libtiff4-devlibgif-dev libncurses5-dev libgtk2.0-dev libxpm-dev libgif-devlibtiff4-dev librsvg2-dev libdbus-1-dev libgconf2-dev -y

* 编译 Emacs:
export CFLAGS="`pkg-config --cflags webkitgtk-3.0 ` -DHAVE_WEBKIT_OSR -g"
export LDFLAGS=`pkg-config --libs webkitgtk-3.0 `
./configure --with-x-toolkit=gtk3
make
sudo make install

* 安装 Deepin Emacs 运行依赖:
sudo aptitude install gdb w3m w3m-img aspell aspell-en texinfo unclutter exuberant-ctags cscope global mplayer2 mp3info libnotify-bin imagemagick wget ghostscript ghostscript-x ghostscript-doc gs-cjk-resource findutils zsh archmage scrot xtrlock git git-core gnutls-bin stardict sdcv ttf-freefont mairix getmail4 msmtp leafnode libotf0 libm17n-0 pyflakes npm -y && npm install jslint

* 安装 Deepin Emacs:
cd ./deepin-emacs/
cp .emacs ~/

cd ./lazycat-lisp/ 

mkdir -p /usr/share/deepin-emacs/
sudo cp -r Document Image Mail Site-Lisp /usr/share/deepin-emacs/

mkdir -p ~/.emacs.d/deepin-emacs/
cp -r Configure-File Gnus ~/.emacs.d/deepin-emacs/

5> 启动 emacs 开始玩吧! :)

其他注意事项:

* Deepin Emacs 个人的使用数据保存在目录:
  ~/.emacs.d

* Hacking Deepin Emacs:
sudo rm -r /usr/share/deepin-emacs && sudo ln -s ~/deepin-emacs/lazycat-lisp /usr/share/deepin-emacs

* 如果要在Emacs里面使用IBus输入法， 按照下面方法做：
1. 建立 ~/.Xresources 这个文件， 并在文件中写入：
   Emacs*useXIM: false
2. 为了让 ~/.Xresources 的设置生效， 重新启动 X 或者用下面命令：
   xrdb ~/.Xresources
3. 修改 IBus 的按键切换， 把 Ctrl + Space 改为 Ctrl + \
