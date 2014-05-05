# Deepin Emacs

* Download emacs git code:

<pre><code>
git clone git://git.savannah.gnu.org/emacs.git
</pre></code>

* Compile emacs git:

<pre><code>
sudo apt-get install build-essential git autoconf texinfo libxaw7-dev libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev libncurses5-dev libxpm-dev libdbus-1-dev libgtk-3-dev w3m w3m-img -y
sudo mkdir -p /usr/share/deepin-emacs/common
cd ./emacs-git && ./autogen.sh
./configure --prefix=/usr/share/deepin-emacs/common --with-x-toolkit=gtk3 && make && sudo make install
</pre></code>

* Install Deepin emacs:
<pre><code>
sudo cp ./site-start.el /usr/share/deepin-emacs/common/share/emacs/site-lisp/
sudo cp -r ./site-lisp /usr/share/deepin-emacs
sudo ln -s /usr/share/deepin-emacs/common/bin/emacs /usr/bin/deepin-emacs
sudo cp ./deepin-emacs.desktop /usr/share/applications/
</pre></code>

* Install depend for python auto-complete:
<pre><code>
sudo apt-get install python-pip
sudo pip install jedi epc mocker
</pre></code>
