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

import Xlib.display
import Xlib.X
import Xlib.XK
import Xlib.protocol.event
from xutils import get_xlib_display

special_X_keysyms = {
    ' ' : "space",
    '\t' : "Tab",
    '\n' : "Return",  # for some reason this needs to be cr, not lf
    '\r' : "Return",
    '\e' : "Escape",
    '!' : "exclam",
    '#' : "numbersign",
    '%' : "percent",
    '$' : "dollar",
    '&' : "ampersand",
    '"' : "quotedbl",
    '\'' : "apostrophe",
    '(' : "parenleft",
    ')' : "parenright",
    '*' : "asterisk",
    '=' : "equal",
    '+' : "plus",
    ',' : "comma",
    '-' : "minus",
    '.' : "period",
    '/' : "slash",
    ':' : "colon",
    ';' : "semicolon",
    '<' : "less",
    '>' : "greater",
    '?' : "question",
    '@' : "at",
    '[' : "bracketleft",
    ']' : "bracketright",
    '\\' : "backslash",
    '^' : "asciicircum",
    '_' : "underscore",
    '`' : "grave",
    '{' : "braceleft",
    '|' : "bar",
    '}' : "braceright",
    '~' : "asciitilde"
    }

def get_keysym(ch):
    keysym = Xlib.XK.string_to_keysym(ch)
    if keysym == 0:
        # Unfortunately, although this works to get the correct keysym
        # i.e. keysym for '#' is returned as "numbersign"
        # the subsequent display.keysym_to_keycode("numbersign") is 0.
        if ch in special_X_keysyms:
            special = special_X_keysyms[ch]
            keysym = Xlib.XK.string_to_keysym(special)
    return keysym

def is_shifted(ch):
    if ch.isupper():
        return True
    if ch in '~!@#$%^&*()_+{}|:\"<>?':
        return True
    return False

def char_to_keycode(ch):
    xlib_display = get_xlib_display()
    
    keysym = get_keysym(ch)
    keycode = xlib_display.keysym_to_keycode(keysym)

    return keycode, is_shifted(ch)

def send_string(window, str, press=True):
    xlib_display = get_xlib_display()

    mask = 0

    if str == "Ctrl":
        keycode = xlib_display.keysym_to_keycode(Xlib.XK.XK_Control_L)
        mask |= Xlib.X.ControlMask
    elif str == "Alt":
        keycode = xlib_display.keysym_to_keycode(Xlib.XK.XK_Alt_L)
        mask |= Xlib.X.Mod1Mask
    elif str == "Shift":
        keycode = xlib_display.keysym_to_keycode(Xlib.XK.XK_Shift_L)
        mask |= Xlib.X.ShiftMask
    elif str == "Super":
        keycode = xlib_display.keysym_to_keycode(Xlib.XK.XK_Super_L)
        mask |= Xlib.X.Mod4Mask
    else:
        keycode, is_shifted = char_to_keycode(str)
        if keycode == 0:
            keycode, is_shifted = char_to_keycode('_')
            
        if is_shifted:
            mask |= Xlib.X.ShiftMask
            
    if press:        
        event_type = Xlib.protocol.event.KeyPress
    else:
        event_type = Xlib.protocol.event.KeyRelease
        
    print str, keycode, mask
        
    event = event_type(
        root=xlib_display.screen().root,
        window=window,
        child=Xlib.X.NONE,
        same_screen=1,
        root_x=1,
        root_y=1,
        event_x=1,
        event_y=1,
        state=mask,
        detail=keycode,
        time=Xlib.X.CurrentTime,
    )
    window.send_event(event, propagate=True)

if __name__ == "__main__":
    xlib_display = get_xlib_display()
    xwindow = xlib_display.create_resource_object("window", 71303255)
    send_string(xwindow, "Alt")
    send_string(xwindow, "x")
    send_string(xwindow, "x", False)
    send_string(xwindow, "Alt", False)
    xlib_display.sync()
