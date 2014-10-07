#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2014 Andy Stewart
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

from PyQt5 import QtCore
from PyQt5 import QtGui
from PyQt5.QtWidgets import QWidget
from PyQt5.QtCore import Qt, QTimer, QEvent
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QPainter
import time
import functools
from xutils import get_xlib_display, grab_focus
from dbus.mainloop.glib import DBusGMainLoop
import dbus
import dbus.service

MINIBUFFER_TRAY_DBUS_NAME = "com.deepin.minibuffer_tray"
MINIBUFFER_TRAY_OBJECT_NAME = "/com/deepin/minibuffer_tray"

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
            
class MinibufferTray(dbus.service.Object):
    def __init__(self, emacs_xid, minibuffer_height):
        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(MINIBUFFER_TRAY_DBUS_NAME, bus=dbus.SessionBus()),
            MINIBUFFER_TRAY_OBJECT_NAME            
        )
        
        self.tray_view = TrayView(emacs_xid, minibuffer_height)
        self.tray_view.show()
    
    @dbus.service.method(MINIBUFFER_TRAY_DBUS_NAME, in_signature="", out_signature="")
    def show(self):
        self.tray_view.show()
        
    @dbus.service.method(MINIBUFFER_TRAY_DBUS_NAME, in_signature="", out_signature="")
    def hide(self):
        self.tray_view.hide()
        
    @dbus.service.method(MINIBUFFER_TRAY_DBUS_NAME, in_signature="ssss", out_signature="")
    def set_minibuffer_allocation(self, x, y, w, h):
        self.tray_view.set_minibuffer_allocation(x, y, w, h)
        self.tray_view.update_allocation(self.tray_view.get_string_width())
        
    @dbus.service.method(MINIBUFFER_TRAY_DBUS_NAME, in_signature="sss", out_signature="")
    def update_pos(self, row, column, line_number):
        self.tray_view.update_pos(row, column, line_number)
        
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
        
        self.update_time_timer = QTimer()
        self.update_time_timer.timeout.connect(self.update_time)
        self.update_time_timer.start(1000)
        
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
        self.minibuffer_h = 0
        
    def eventFilter(self, obj, event):
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease, QEvent.InputMethodQuery, QEvent.KeyPress, QEvent.KeyRelease, QEvent.Enter, QEvent.WindowActivate, QEvent.ActivationChange, QEvent.ToolTip, QEvent.Leave]:
            grab_focus(self.emacs_xid)
            
            return True
        
    def set_minibuffer_allocation(self, x, y, w, h):
        (self.minibuffer_x, self.minibuffer_y, self.minibuffer_w, self.minibuffer_h) = (x, y, w, h)
        
    def get_current_time(self):
        return time.strftime("%H:%M %A", time.localtime())

    def update_info(self):
        info_string = "%s   %s   %s" % (self.pos_string, self.percent_string, self.time_string)
        
        if self.info_string != info_string:
            self.info_string = info_string
            
            info_width = self.get_string_width()
            if self.info_width != info_width:        
                self.info_width = info_width
                self.update_allocation(self.info_width)
            
            self.update()
        
    def update_allocation(self, width):
        self.resize(width, self.minibuffer_height)
        self.reparent(
            self.minibuffer_x + self.minibuffer_w - width,
            self.minibuffer_y + self.minibuffer_h - self.minibuffer_height,
        )
    
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
        painter.setPen(QtGui.QColor(19, 125, 17, 255))
        painter.setFont(self.font)
        painter.drawText(0, 0, self.width(), self.height(), QtCore.Qt.AlignRight, self.info_string)
        painter.end()
            
    def reparent(self, x, y):
        xlib_display = get_xlib_display()
        
        tray_xwindow = xlib_display.create_resource_object("window", self.winId().__int__())
        emacs_xwindow = xlib_display.create_resource_object("window", self.emacs_xid)
        
        tray_xwindow.reparent(emacs_xwindow, x, y + 1)
        
        xlib_display.sync()
        
if __name__ == "__main__":
    import sys
    import signal
    
    DBusGMainLoop(set_as_default=True) # WARING: only use once in one process
    
    bus = dbus.SessionBus()
    if bus.request_name(MINIBUFFER_TRAY_DBUS_NAME) != dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER:
        print("Minibuffer tray process has startup.")
    else:
        app = QApplication(sys.argv)
        
        tray = MinibufferTray(sys.argv[1], sys.argv[2])
        
        print("Minibuffer tray process start.")
        
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        sys.exit(app.exec_())
