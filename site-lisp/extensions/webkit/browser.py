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
from PyQt5.QtCore import QCoreApplication, QEvent
if os.name == 'posix':
    QCoreApplication.setAttribute(QtCore.Qt.AA_X11InitThreads, True)
    
from PyQt5.QtWebKitWidgets import QWebView, QWebPage
from PyQt5.QtWebKit import  QWebSettings
from PyQt5.QtWidgets import QApplication
from PyQt5.QtCore import QUrl, Qt
from PyQt5 import QtGui
import time
import os
from epc.server import ThreadingEPCServer
import threading
from PyQt5.QtWidgets import QWidget
from PyQt5.QtGui import QPainter, QImage
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
            
class BrowserBuffer(QWebView):

    redrawScreenshot = QtCore.pyqtSignal(object)
    
    def __init__(self, buffer_id, buffer_width, buffer_height):
        super(BrowserBuffer, self).__init__()
        
        self.buffer_id = buffer_id
        self.buffer_width = buffer_width
        self.buffer_height = buffer_height
        
        self.page().setLinkDelegationPolicy(QWebPage.DelegateAllLinks)
        self.page().linkClicked.connect(self.link_clicked)
        self.page().mainFrame().setScrollBarPolicy(Qt.Horizontal, Qt.ScrollBarAlwaysOff)
        self.settings().setUserStyleSheetUrl(QUrl.fromLocalFile(os.path.join(get_parent_dir(__file__), "scrollbar.css")))
        self.settings().setAttribute(QWebSettings.PluginsEnabled, True)
        
        self.adjust_size(self.buffer_width, self.buffer_height)
        
        self.view_dict = {}
        
        self.press_ctrl_flag = False
        
    def eventFilter(self, obj, event):
        if event.type() in [QEvent.KeyPress, QEvent.KeyRelease,
                            QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                            QEvent.MouseMove, QEvent.MouseButtonDblClick, QEvent.Wheel,
                            QEvent.InputMethod, QEvent.InputMethodQuery, QEvent.ShortcutOverride,
                            QEvent.ActivationChange, QEvent.Enter, QEvent.WindowActivate,
                            ]:
            QApplication.sendEvent(self, event)
            
            if event.type() == QEvent.KeyPress and event.key() == QtCore.Qt.Key_Control:
                self.press_ctrl_flag = True
                call_message("Ctrl press")
            elif event.type() == QEvent.KeyRelease and event.key() == QtCore.Qt.Key_Control:
                self.press_ctrl_flag = False
                call_message("Ctrl release")
        else:
            if event.type() not in [12, 77]:
                print event.type(), event
        
        return False
    
    def add_view(self, view_id, emacs_xid, x, y, w, h):
        view = BrowserView(self, view_id)
        self.view_dict[view_id] = view
        self.update_view(view_id, emacs_xid, x, y, w, h)
        
        view.show()
        
        call_message("Add view %s" % view_id)
        
    def remove_view(self, view_id):
        if self.view_dict.has_key(view_id):
            self.view_dict[view_id].remove()
            self.view_dict.pop(view_id)
        
    def update_view(self, view_id, emacs_xid, x, y, w, h):
        self.view_dict[view_id].moveresize(emacs_xid, x, y, w, h)
        
    def remove_all_views(self):
        for view_id in self.view_dict.keys():
            self.remove_view(view_id)
        
    def adjust_size(self, width, height):
        self.buffer_width = width
        self.buffer_height = height
        self.resize(self.buffer_width, self.buffer_height)
        
    @postGui()
    def redraw(self):
        if len(self.view_dict) > 0:
            qimage = QImage(self.buffer_width, self.buffer_height, QImage.Format_ARGB32)
            self.render(qimage)
        
            self.redrawScreenshot.emit(qimage)
        
    @postGui()    
    def open_url(self, url):    
        self.load(QUrl(url))
        
    def link_clicked(self, url):
        if self.press_ctrl_flag:
            call_open_url(url.url())
        else:
            self.load(url)
        
class BrowserView(QWidget):
    def __init__(self, browser_buffer, view_id):
        super(BrowserView, self).__init__()
        
        self.browser_buffer = browser_buffer
        self.view_id = view_id
        
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_TranslucentBackground, True)
        
        self.setContentsMargins(0, 0, 0, 0)
        
        self.browser_buffer.redrawScreenshot.connect(self.updateView)
        
        self.qimage = None
        
        self.installEventFilter(browser_buffer)
        
    def remove(self):
        self.browser_buffer.redrawScreenshot.disconnect(self.updateView)
        self.destroy()
        
    def paintEvent(self, event):    
        if self.qimage:
            painter = QPainter(self)
            painter.drawImage(QtCore.QRect(0, 0, self.width(), self.height()), self.qimage)
            painter.end()
        else:
            painter = QPainter(self)
            painter.setBrush(QtGui.QColor(255, 255, 255, 255))
            painter.drawRect(0, 0, self.width(), self.height())
            painter.end()
        
    @postGui()
    def updateView(self, qimage):
        self.qimage = qimage
        self.update()
        
    def moveresize(self, emacs_xid, x, y, w, h):
        self.resize(w, h)
        self.reparent(emacs_xid, x, y)
        
    def adjust_size(self, emacs_xid, x, y, w, h):
        self.moveresize(emacs_xid, x, y, w, h)
        self.browser_buffer.adjust_size(w, h)
        
        call_message("Adjust size %s" % self.view_)
        
    def reparent(self, emacs_xid, x, y):
        from Xlib import display
        xlib_display = display.Display()
        
        browser_xid = self.winId().__int__()
        browser_xwindow = xlib_display.create_resource_object("window", int(browser_xid))
        emacs_xwindow = xlib_display.create_resource_object("window", int(emacs_xid))
        
        browser_xwindow.reparent(emacs_xwindow, x, y)
        xlib_display.sync()
        
if __name__ == '__main__':
    import sys
    import signal
    
    app = QApplication(sys.argv)
    
    server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
    
    server_thread = threading.Thread(target=server.serve_forever)
    server_thread.allow_reuse_address = True
    
    buffer_dict = {}
    
    def call_open_url(url):
        handler = server.clients[0]
        handler.call('open-url', [url])
    
    def call_message(message):
        handler = server.clients[0]
        handler.call('message', [message])
    
    # NOTE: every epc method must should wrap with postGui.
    # Because epc server is running in sub-thread.
    @postGui(False)        
    def create_buffer(buffer_id, buffer_url, buffer_width, buffer_height):
        if not buffer_dict.has_key(buffer_id):
            buffer = BrowserBuffer(buffer_id, buffer_width, buffer_height)
            buffer.open_url(buffer_url)
            buffer_dict[buffer_id] = buffer
            
    @postGui(False)        
    def remove_buffer(buffer_id):
        if buffer_dict.has_key(buffer_id):
            buffer = buffer_dict[buffer_id]
            buffer.remove_all_views()
            buffer_dict.pop(buffer_id)
            buffer.destroy()
            
            call_message("Remove buffer %s" % buffer_id)
            
    @postGui(False)        
    def adjust_view(emacs_xid, buffer_id, view_id, x, y, w, h):
        call_message("****************** %s %s" % (buffer_dict.has_key(buffer_id), buffer_dict[buffer_id].view_dict.has_key(view_id)))
        if buffer_dict.has_key(buffer_id) and buffer_dict[buffer_id].view_dict.has_key(view_id):
            buffer_dict[buffer_id].view_dict[view_id].adjust_size(emacs_xid, x, y, w, h)
            
    @postGui(False)        
    def update_views(emacs_xid, view_infos):
        buffer_view_dict = {}
        
        for view_info in view_infos:
            [buffer_id, x, y, w, h] = view_info
            view_id = "%s_%s" % (x, y)
            if buffer_dict.has_key(buffer_id):
                if not buffer_view_dict.has_key(buffer_id):
                    buffer_view_dict[buffer_id] = {}
                    
                buffer_view_dict[buffer_id][view_id] = (x, y, w, h)
            else:
                call_message("Buffer id %s is not exist!" % buffer_id)
                
        for buffer in buffer_dict.values():
            if buffer_view_dict.has_key(buffer.buffer_id):
                emacs_view_ids = buffer_view_dict[buffer.buffer_id].keys()
                buffer_view_ids = buffer.view_dict.keys()
                
                for emacs_view_id in emacs_view_ids:
                    (x, y, w, h) = buffer_view_dict[buffer.buffer_id][emacs_view_id]
                    # Update view.
                    if emacs_view_id in buffer_view_ids:
                        buffer.update_view(emacs_view_id, emacs_xid, x, y, w, h)

                    # Create view.
                    else:
                        buffer.add_view(emacs_view_id, emacs_xid, x, y, w, h)
                for buffer_view_id in buffer_view_ids:
                    # Remove view.
                    if buffer_view_id not in emacs_view_ids:
                        buffer.remove_view(buffer_view_id)
            else:
                buffer.remove_all_views()
    
    def update_buffer():
        while True:
            for buffer in buffer_dict.values():
                buffer.redraw()
            
            time.sleep(0.05)
            
    server_thread.start()
    server.print_port()
    
    server.register_function(create_buffer)
    server.register_function(remove_buffer)
    server.register_function(adjust_view)
    server.register_function(update_views)
    
    threading.Thread(target=update_buffer).start()            
    
    # This function just for test python module.
    def test():
        emacs_xid = "65011799"
        
        create_buffer("1", "http://www.google.com", 1600, 400)

        # View will adjust.
        buffer_dict["1"].add_view("400_500", emacs_xid, 400, 500, 300, 400)

        # View will destory.
        buffer_dict["1"].add_view("900_500", emacs_xid, 900, 500, 300, 400)

        # View will add.
        view_infos = [
            ["1", 0, 0, 300, 400],
            ["1", 0, 500, 300, 400],
            ["1", 400, 0, 300, 400],
            ["1", 400, 500, 500, 400],
        ]
        
        update_views(emacs_xid, view_infos)
        
        adjust_view(emacs_xid, "1", "400_500", 400, 500, 500, 400)
        
    # test()    
        
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
