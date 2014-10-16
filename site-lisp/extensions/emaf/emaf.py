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
    
from PyQt5.QtQuick import QQuickView
from PyQt5.QtGui import QSurfaceFormat, QColor
from PyQt5 import QtCore, QtQuick

import sys
from PyQt5.QtWidgets import QApplication, qApp
import signal

from dbus.mainloop.glib import DBusGMainLoop
import dbus
import dbus.service
import functools
from xutils import get_xlib_display

EMAF_DBUS_NAME = "com.deepin.emaf"
EMAF_OBJECT_NAME = "/com/deepin/emaf"

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
            
class Emaf(dbus.service.Object):
    def __init__(self, emacs_xid, init_width, init_height, view):
        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(EMAF_DBUS_NAME, bus=dbus.SessionBus()),
            EMAF_OBJECT_NAME            
        )
        
        self.view = view
        self.emacs_xid = int(emacs_xid)
        self.frame_width = int(init_width)
        self.frame_height = int(init_height)
        self.view.show()
        
        self.update_allocation()
        
    def reparent(self):
        xlib_display = get_xlib_display()
        
        emaf_xwindow = xlib_display.create_resource_object("window", self.view.winId().__int__())
        emacs_xwindow = xlib_display.create_resource_object("window", self.emacs_xid)
        
        emaf_xwindow.reparent(emacs_xwindow, 0, 0)
        
        xlib_display.sync()
        
    def update_allocation(self):
        self.view.resize(self.frame_width, self.frame_height)
        self.reparent()
    
    @dbus.service.method(EMAF_DBUS_NAME, in_signature="", out_signature="")
    def show(self):
        self.view.show()
        
    @dbus.service.method(EMAF_DBUS_NAME, in_signature="", out_signature="")
    def hide(self):
        self.view.hide()
        
    @dbus.service.method(EMAF_DBUS_NAME, in_signature="ss", out_signature="")
    def update_frame_size(self, w, h):
        self.frame_width = int(w)
        self.frame_height = int(h)
        self.update_allocation()
        
class Window(QQuickView):

    def __init__(self):
        QQuickView.__init__(self)
        
        surface_format = QSurfaceFormat()
        surface_format.setAlphaBufferSize(8)
        
        self.setColor(QColor(0, 0, 0, 0))
        self.setFlags(QtCore.Qt.FramelessWindowHint)
        self.setResizeMode(QtQuick.QQuickView.SizeRootObjectToView)
        self.setFormat(surface_format)
        
        self.qml_context = self.rootContext()
            
    def exit_app(self):
        qApp.quit()
        
if __name__ == "__main__":
    DBusGMainLoop(set_as_default=True) # WARING: only use once in one process
    
    bus = dbus.SessionBus()
    if bus.request_name(EMAF_DBUS_NAME) != dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER:
        print("Minibuffer tray process has startup.")
    else:
        app = QApplication(sys.argv)
        view = Window()
        
        emaf = Emaf(sys.argv[1], sys.argv[2], sys.argv[3], view)
        emaf.update_allocation()
        
        qApp.lastWindowClosed.connect(view.exit_app)
        
        qml_context = view.rootContext()
        qml_context.setContextProperty("windowView", view)
        qml_context.setContextProperty("qApp", qApp)
            
        view.setSource(QtCore.QUrl.fromLocalFile(os.path.join(os.path.dirname(__file__), 'emaf.qml')))
            
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        sys.exit(app.exec_())
