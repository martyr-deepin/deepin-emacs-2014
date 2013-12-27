Deepin Emacs

* Compile emacs git

sudo apt-get install build-essential git autoconf texinfo libxaw7-dev libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev libncurses5-dev libxpm-dev libdbus-1-dev libgtk-3-dev -y
cd ./emacs-git
./autogen.sh
./configure --prefix=/home/andy/emacs-bin --with-x-toolkit=gtk3
make
sudo make install

http://dynamic-thinking.blogspot.com/2012/07/emacs-speeding-up-loading-your-emacs.html
