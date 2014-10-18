#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2014 Deepin, Inc.
#               2014 Andy Stewart
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
    
from PyQt5 import QtCore, QtQuick
from PyQt5.QtGui import QSurfaceFormat, QColor
from PyQt5.QtQuick import QQuickView
from PyQt5.QtWidgets import QApplication, qApp
from dbus.mainloop.glib import DBusGMainLoop
from constant import EMAF_DBUS_NAME, EMAF_OBJECT_NAME
from xutils import get_xlib_display
import dbus
import dbus.service
import signal
import sys

class Emaf(dbus.service.Object):
    def __init__(self, frame):
        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(EMAF_DBUS_NAME, bus=dbus.SessionBus()),
            EMAF_OBJECT_NAME            
        )
        
        self.frame = frame
        
    @dbus.service.method(EMAF_DBUS_NAME, in_signature="", out_signature="")
    def show(self):
        self.frame.show()
        
    @dbus.service.method(EMAF_DBUS_NAME, in_signature="", out_signature="")
    def hide(self):
        self.frame.hide()
        
    @dbus.service.method(EMAF_DBUS_NAME, in_signature="ss", out_signature="")
    def update_frame_size(self, w, h):
        self.frame.update_size(w, h)
        
class Frame(QQuickView):

    def __init__(self, emacs_xid, init_width, init_height):
        QQuickView.__init__(self)
        
        surface_format = QSurfaceFormat()
        surface_format.setAlphaBufferSize(8)
        
        self.setColor(QColor(0, 0, 0, 0))
        self.setFlags(QtCore.Qt.FramelessWindowHint)
        self.setResizeMode(QtQuick.QQuickView.SizeRootObjectToView)
        self.setFormat(surface_format)
        
        self.qml_context = self.rootContext()
        
        self.emacs_xid = int(emacs_xid)
        self.emacs_width = int(init_width)
        self.emacs_height = int(init_height)
        
        self.show()
            
    def reparent(self):
        xlib_display = get_xlib_display()
        
        emaf_xwindow = xlib_display.create_resource_object("window", self.winId().__int__())
        emacs_xwindow = xlib_display.create_resource_object("window", self.emacs_xid)
        
        emaf_xwindow.reparent(emacs_xwindow, 0, 0)
        
        xlib_display.sync()
        
    def update_allocation(self):
        self.resize(self.emacs_width, self.emacs_height)
        self.reparent()
        
    def update_size(self, w, h):
        self.emacs_width = w
        self.emacs_height = h
        self.update_allocation()
    
    def exit_app(self):
        qApp.quit()
        
if __name__ == "__main__":
    DBusGMainLoop(set_as_default=True) # WARING: only use once in one process
    
    bus = dbus.SessionBus()
    if bus.request_name(EMAF_DBUS_NAME) != dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER:
        print("Minibuffer tray process has startup.")
    else:
        app = QApplication(sys.argv)
        emacs_xid, init_width, init_height = sys.argv[1], sys.argv[2], sys.argv[3]
        frame = Frame(emacs_xid, init_width, init_height)
        emaf = Emaf(frame)
        
        qApp.lastWindowClosed.connect(frame.exit_app)
        
        qml_context = frame.rootContext()
        qml_context.setContextProperty("frame", frame)
        qml_context.setContextProperty("qApp", qApp)
            
        frame.setSource(QtCore.QUrl.fromLocalFile(os.path.join(os.path.dirname(__file__), 'emaf.qml')))
        frame.update_allocation()
            
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        sys.exit(app.exec_())
