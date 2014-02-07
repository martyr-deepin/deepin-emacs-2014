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
from utils import get_parent_dir
from xutils import get_xlib_display, grab_focus, ActiveWindowWatcher, get_parent_window_id
from send_key import send_string

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
            
class WebPage(QWebPage):
    def __init__(self):
        super(WebPage, self).__init__()
 
    def acceptNavigationRequest(self, frame, request, type):
        if(type == QWebPage.NavigationTypeLinkClicked):
            if(frame == self.mainFrame()):
                self.view().link_clicked(request.url())
            else:
                call_method("open-url", [request.url().toString()])
                return False
            
        return QWebPage.acceptNavigationRequest(self, frame, request, type)            
            
class BrowserBuffer(QWebView):

    redrawScreenshot = QtCore.pyqtSignal(object)
    
    def __init__(self, buffer_id, buffer_width, buffer_height):
        super(BrowserBuffer, self).__init__()
        
        self.buffer_id = buffer_id
        self.buffer_width = buffer_width
        self.buffer_height = buffer_height
        
        # self.setPage(WebPage())
        
        self.page().setLinkDelegationPolicy(QWebPage.DelegateAllLinks)
        self.page().linkClicked.connect(self.link_clicked)
        self.page().mainFrame().setScrollBarPolicy(Qt.Horizontal, Qt.ScrollBarAlwaysOff)
        self.settings().setUserStyleSheetUrl(QUrl.fromLocalFile(os.path.join(get_parent_dir(__file__), "theme.css")))
        self.settings().setAttribute(QWebSettings.PluginsEnabled, True)
        self.settings().setAttribute(QWebSettings.JavascriptEnabled, True)
        self.settings().setAttribute(QWebSettings.JavascriptCanOpenWindows, True)
        
        self.adjust_size(self.buffer_width, self.buffer_height)
        
        self.view_dict = {}
        
        self.titleChanged.connect(self.change_title)
        
        self.press_ctrl_flag = False
        
    def change_title(self, title):
        call_method("change-buffer-title", [self.buffer_id, title])
        
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
            elif event.type() == QEvent.KeyRelease and event.key() == QtCore.Qt.Key_Control:
                self.press_ctrl_flag = False
                
            global emacs_xwindow_id
            
            xlib_display = get_xlib_display()
            xwindow = xlib_display.create_resource_object("window", emacs_xwindow_id)
            
            mask = []
            event_key = event.text()            
            if event.modifiers() & QtCore.Qt.AltModifier == QtCore.Qt.AltModifier:
                mask.append("Alt")
            elif event.modifiers() & QtCore.Qt.ControlModifier == QtCore.Qt.ControlModifier:
                mask.append("Ctrl")
            elif event.modifiers() & QtCore.Qt.ShiftModifier == QtCore.Qt.ShiftModifier:
                mask.append("Shift")
            elif event.modifiers() & QtCore.Qt.MetaModifier == QtCore.Qt.MetaModifier:
                mask.append("Super")
                
            send_string(xwindow, event_key, mask, event.type() == QEvent.KeyPress)
            
            xlib_display.sync()
        else:
            if event.type() not in [12, 77]:
                call_method("%s %s" % (event.type(), event))
        
        return False
    
    def add_view(self, view_id, x, y, w, h):
        view = BrowserView(self, view_id)
        self.view_dict[view_id] = view
        self.update_view(view_id, x, y, w, h)
        
        view.show()
        
    def remove_view(self, view_id):
        if self.view_dict.has_key(view_id):
            self.view_dict[view_id].remove()
            self.view_dict.pop(view_id)
        
    def update_view(self, view_id, x, y, w, h):
        self.view_dict[view_id].moveresize(x, y, w, h)
        
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
            call_method("open-url", [url.url()])
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
        
    def moveresize(self, x, y, w, h):
        self.resize(w, h)
        self.reparent(x, y)
        
    def adjust_size(self, x, y, w, h):
        self.moveresize(x, y, w, h)
        self.browser_buffer.adjust_size(w, h)
        
    def reparent(self, x, y):
        xlib_display = get_xlib_display()
        
        browser_xwindow_id = self.winId().__int__()
        browser_xwindow = xlib_display.create_resource_object("window", browser_xwindow_id)
        emacs_xwindow = xlib_display.create_resource_object("window", emacs_xwindow_id)
        
        browser_xwindow.reparent(emacs_xwindow, x, y)
        
        xlib_display.sync()
        
if __name__ == '__main__':
    import sys
    import signal
    
    app = QApplication(sys.argv)
    
    server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
    
    server_thread = threading.Thread(target=server.serve_forever)
    server_thread.allow_reuse_address = True
    
    emacs_xwindow_id = 0
    
    buffer_dict = {}
    
    def call_message(message):
        call_method("message", [message])
        
    def call_method(method_name, args):
        handler = server.clients[0]
        handler.call(method_name, args)
        
    def handle_active_window(active_window_id):
        global emacs_xwindow_id
        
        emacs_real_id = get_parent_window_id(emacs_xwindow_id)
        
        call_method("message", ["handle_active_window: %s %s %s" % (active_window_id, emacs_xwindow_id, emacs_real_id)])
        
        if active_window_id == emacs_real_id:
            call_method("focus-browser-view", [])
        
    @postGui(False)    
    def init(emacs_xid):
        global emacs_xwindow_id
        
        emacs_xwindow_id = int(emacs_xid)
        
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
    def adjust_size(buffer_id, w, h):
        if buffer_dict.has_key(buffer_id):
            buffer_dict[buffer_id].adjust_size(w, h)
            
    @postGui(False)        
    def update_views(view_infos):
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
                        buffer.update_view(emacs_view_id, x, y, w, h)

                    # Create view.
                    else:
                        buffer.add_view(emacs_view_id, x, y, w, h)
                for buffer_view_id in buffer_view_ids:
                    # Remove view.
                    if buffer_view_id not in emacs_view_ids:
                        buffer.remove_view(buffer_view_id)
            else:
                buffer.remove_all_views()
                
    @postGui(False)            
    def focus_view(buffer_id, x, y, w, h):
        if buffer_dict.has_key(buffer_id):
            buffer = buffer_dict[buffer_id]
            view_id = "%s_%s" % (x, y)
            
            if buffer.view_dict.has_key(view_id):
                view = buffer.view_dict[view_id]
                view_xwindow_id = view.winId().__int__()
                grab_focus(view_xwindow_id)
                
    def update_buffer():
        while True:
            for buffer in buffer_dict.values():
                buffer.redraw()
            
            time.sleep(0.05)
            
    server_thread.start()
    server.print_port()
    
    server.register_function(init)
    server.register_function(create_buffer)
    server.register_function(remove_buffer)
    server.register_function(adjust_size)
    server.register_function(update_views)
    server.register_function(focus_view)
    
    threading.Thread(target=update_buffer).start()            
    
    active_window_watcher = ActiveWindowWatcher()
    active_window_watcher.activeWindowChanged.connect(handle_active_window)
    active_window_watcher.start()        
    
    # This function just for test python module.
    def test():
        emacs_xid = "106954839"
        
        init(emacs_xid)
        
        create_buffer("1", "http://www.google.com", 1600, 400)

        # View will adjust.
        buffer_dict["1"].add_view("400_500", 400, 500, 300, 400)

        # View will destory.
        buffer_dict["1"].add_view("900_500", 900, 500, 300, 400)

        # View will add.
        view_infos = [
            ["1", 0, 0, 300, 400],
            ["1", 0, 500, 300, 400],
            ["1", 400, 0, 300, 400],
            ["1", 400, 500, 500, 400],
        ]
        
        update_views(view_infos)
        
        adjust_size("1", "400_500", 400, 500, 500, 400)
        
    # test()    
        
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
