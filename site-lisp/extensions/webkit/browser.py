#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011 ~ 2014 Andy Stewart
# 
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
from PyQt5 import QtCore
from PyQt5.QtCore import QCoreApplication
if os.name == 'posix':
    QCoreApplication.setAttribute(QtCore.Qt.AA_X11InitThreads, True)
    
from PyQt5.QtWebKitWidgets import QWebView, QWebPage
from PyQt5.QtWidgets import QApplication
from PyQt5.QtCore import QUrl, Qt
import os
from epc.server import ThreadingEPCServer
import threading
from PyQt5.QtWidgets import QWidget, QVBoxLayout
import functools

def get_parent_dir(filepath, level=1):
    '''
    Get parent directory with given return level.
    
    @param filepath: Filepath.
    @param level: Return level, default is 1
    @return: Return parent directory with given return level. 
    '''
    parent_dir = os.path.realpath(filepath)
    
    while(level > 0):
        parent_dir = os.path.dirname(parent_dir)
        level -= 1
    
    return parent_dir

class postGui(QtCore.QObject):
    
    throughThread = QtCore.pyqtSignal(object, object)    
    
    def __init__(self, inclass=True):
        super(postGui, self).__init__()
        self.throughThread.connect(self.onSignalReceived)
        self.inclass = inclass
        
    def __call__(self, func):
        self._func = func
        
        @functools.wraps(func)
        def objCall(*args, **kwargs):
            self.emitSignal(args, kwargs)
        return objCall
        
    def emitSignal(self, args, kwargs):
        self.throughThread.emit(args, kwargs)
                
    def onSignalReceived(self, args, kwargs):
        if self.inclass:
            obj, args = args[0], args[1:]
            self._func(obj, *args, **kwargs)
        else:    
            self._func(*args, **kwargs)
            
class Browser(QWidget):
    def __init__(self):
        super(Browser, self).__init__()
        
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_TranslucentBackground, True)
        
        self.setContentsMargins(0, 0, 0, 0)
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.setLayout(self.layout)
        
    @postGui()    
    def open_url(self, url):    
        self.view = QWebView(self)
        self.view.load(QUrl(url))
        self.view.page().setLinkDelegationPolicy(QWebPage.DelegateAllLinks)
        self.view.page().linkClicked.connect(self.link_clicked)
        self.view.page().mainFrame().setScrollBarPolicy(Qt.Horizontal, Qt.ScrollBarAlwaysOff)
            
        self.view.settings().setUserStyleSheetUrl(QUrl.fromLocalFile(os.path.join(get_parent_dir(__file__), "scrollbar.css")))
        self.layout.addWidget(self.view)
        
    @postGui()    
    def moveresize(self, emacs_xid, x, y, w, h):
        self.resize(w, h)
        self.reparent(emacs_xid, x, y)
        
    def reparent(self, emacs_xid, x, y):
        from Xlib import display
        xlib_display = display.Display()
        
        browser_xid = self.winId().__int__()
        browser_xwindow = xlib_display.create_resource_object("window", int(browser_xid))
        emacs_xwindow = xlib_display.create_resource_object("window", int(emacs_xid))
        
        browser_xwindow.reparent(emacs_xwindow, x, y)
        xlib_display.sync()
        
    def link_clicked(self, url):
        self.view.load(url)
        
if __name__ == '__main__':
    import sys
    import signal
    
    app = QApplication(sys.argv)
    
    browser = Browser()
    
    server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
    
    server.register_function(browser.open_url)
    server.register_function(browser.moveresize)
    server.register_function(browser.hide)
    server.register_function(browser.show)
    
    server_thread = threading.Thread(target=server.serve_forever)
    server_thread.allow_reuse_address = True
    
    server_thread.start()
    server.print_port()
    
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
