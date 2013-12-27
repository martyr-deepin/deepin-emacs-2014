Deepin Emacs

* Compile emacs git

sudo apt-get install build-essential git autoconf
cd ./emacs-git
./autogen.sh
./configure --prefix=/home/andy/emacs-bin
