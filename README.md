Deepin Emacs

* Compile emacs git:

sudo apt-get install build-essential git autoconf texinfo libxaw7-dev libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev libncurses5-dev libxpm-dev libdbus-1-dev libgtk-3-dev w3m w3m-img -y
sudo mkdir -p /usr/share/deepin-emacs/common
cd ./emacs-git
./autogen.sh
./configure --prefix=/usr/share/deepin-emacs/common --with-x-toolkit=gtk3
make
sudo make install

* Install Deepin emacs:
sudo cp ./site-start.el /usr/share/deepin-emacs/common/share/emacs/site-lisp/
sudo cp -r ./site-lisp /usr/share/deepin-emacs
sudo ln -s /usr/share/deepin-emacs/common/bin/emacs /usr/bin/deepin-emacs
sudo cp ./deepin-emacs.desktop /usr/share/applications/

http://dynamic-thinking.blogspot.com/2012/07/emacs-speeding-up-loading-your-emacs.html

* Install depend for python auto-complete:
sudo apt-get install python-pip
sudo pip install jdei epc mocker