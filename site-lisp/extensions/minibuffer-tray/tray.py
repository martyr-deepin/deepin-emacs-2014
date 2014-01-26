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
    
from PyQt5 import QtGui
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget
from PyQt5.QtCore import Qt, QTimer, QEvent
from PyQt5.QtWidgets import QApplication
from epc.server import ThreadingEPCServer
import threading
import functools
from xutils import get_xlib_display, grab_focus
import time

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
            
class TrayView(QWidget):

    def __init__(self, emacs_xid, minibuffer_height):
        super(TrayView, self).__init__()
        self.emacs_xid = int(emacs_xid)
        self.minibuffer_height = int(minibuffer_height)
        
        self.setWindowFlags(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_TranslucentBackground, True)
        self.setAttribute(Qt.WA_TransparentForMouseEvents, True)
        self.setAttribute(Qt.WA_ShowWithoutActivating, True)
        self.setContentsMargins(0, 0, 0, 0)
        self.setFocusPolicy(Qt.NoFocus)
        
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_time)
        self.timer.start(1000)
        
        self.time_string = self.get_current_time()
        self.pos_string = ""
        self.percent_string = ""
        self.info_string = ""
        self.info_width = 0
        self.padding = 3
        self.font = QtGui.QFont()
        
        self.minibuffer_x = 0
        self.minibuffer_y = 0
        self.minibuffer_w = 0
        
        # self.installEventFilter(self)
        
    def eventFilter(self, obj, event):
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease, QEvent.InputMethodQuery, QEvent.KeyPress, QEvent.KeyRelease, QEvent.Enter, QEvent.WindowActivate, QEvent.ActivationChange, QEvent.ToolTip, QEvent.Leave]:
            grab_focus(self.emacs_xid)
            
            return True
        
    def set_minibuffer_allocation(self, x, y, w):
        (self.minibuffer_x, self.minibuffer_y, self.minibuffer_w) = (x, y, w)
        
    def get_current_time(self):
        return time.strftime("%H:%M %A", time.localtime())

    def update_info(self):
        self.info_string = "%s   %s   %s" % (self.pos_string, self.percent_string, self.time_string)
        info_width = self.get_string_width()
        if self.info_width != info_width:        
            self.info_width = info_width
            
            self.resize(info_width, self.height())
            self.reparent(
                self.minibuffer_x + self.minibuffer_w - info_width,
                self.minibuffer_y,
            )
        
        self.update()
    
    @postGui()
    def update_time(self):
        self.time_string = self.get_current_time()
        
        self.update_info()
        
        return False
    
    def update_pos(self, row, column, line_number):
        if row == 1:
            self.percent_string = "Top"
        elif row - 1 == line_number:
            self.percent_string = "Bottom"
        else:
            percent = int(row * 100 / line_number)
            if percent == 100:
                percent = 99
            self.percent_string = "%s%%" % percent
        self.pos_string = "( %s, %s )" % (row, column)
        
        self.update_info()
        
    def get_string_width(self):
        self.font.setPixelSize(self.height() - self.padding * 2)
        fm = QtGui.QFontMetrics(self.font)
        return fm.width(self.info_string)
        
    def paintEvent(self, event):    
        painter = QPainter(self)
        self.font.setPixelSize(self.height() - self.padding * 2)
        painter.setBrush(QtGui.QColor(0, 0, 0, 255))
        painter.drawRect(0, 0, self.width(), self.height())
        painter.setPen(QtGui.QColor(19, 125, 17, 255))
        painter.setFont(self.font)
        painter.drawText(0, 0, self.width(), self.height(), QtCore.Qt.AlignRight, self.info_string)
        painter.end()
            
    def reparent(self, x, y):
        xlib_display = get_xlib_display()
        
        browser_xid = self.winId().__int__()
        browser_xwindow = xlib_display.create_resource_object("window", browser_xid)
        emacs_xwindow = xlib_display.create_resource_object("window", self.emacs_xid)
        
        browser_xwindow.reparent(emacs_xwindow, x, y)
        
        xlib_display.sync()
            
if __name__ == '__main__':
    import sys
    import signal
    
    app = QApplication(sys.argv)
    
    server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
    
    server_thread = threading.Thread(target=server.serve_forever)
    server_thread.allow_reuse_address = True
    
    tray_view = None
    
    def call_method(method_name, args):
        handler = server.clients[0]
        handler.call(method_name, args)
        
    @postGui(False)    
    def init(emacs_xid, minibuffer_height):
        global tray_view
        
        tray_view = TrayView(emacs_xid, minibuffer_height)
    
    @postGui(False)    
    def show():
        tray_view.show()
        
    @postGui(False)    
    def hide():
        tray_view.hide()
        
    @postGui(False)    
    def set_minibuffer_allocation(x, y, w):
        init_width = tray_view.get_string_width()
        tray_view.set_minibuffer_allocation(x, y, w)
        tray_view.resize(init_width, tray_view.minibuffer_height)
        tray_view.reparent(x + w - init_width, y)
        
    @postGui(False)    
    def update_pos(row, column, line_number):
        tray_view.update_pos(row, column, line_number)
        
    server_thread.start()
    server.print_port()
    
    server.register_function(init)
    server.register_function(hide)
    server.register_function(show)
    server.register_function(set_minibuffer_allocation)
    server.register_function(update_pos)
    
    # tray_view = TrayView(0, 24)
    # show()
    # tray_view.move(300, 300)
    # tray_view.resize(200, 30)
    
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    sys.exit(app.exec_())
