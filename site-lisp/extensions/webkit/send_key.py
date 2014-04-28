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

def send_string(window, str, modifiers, press=True):
    xlib_display = get_xlib_display()

    mask = 0
    for modifier in modifiers:
        if modifier == "Ctrl":
            mask |= Xlib.X.ControlMask
        elif modifier == "Alt":
            mask |= Xlib.X.Mod1Mask
        elif modifier == "Shift":
            mask |= Xlib.X.ShiftMask
        elif modifier == "Super":
            mask |= Xlib.X.Mod4Mask

    keycode = xlib_display.keysym_to_keycode(get_keysym(str))

    if press:
        event_type = Xlib.protocol.event.KeyPress
    else:
        event_type = Xlib.protocol.event.KeyRelease

    event = event_type(
        root=xlib_display.screen().root,
        window=window,
        child=Xlib.X.NONE,
        same_screen=1,
        root_x=0,
        root_y=0,
        event_x=0,
        event_y=0,
        state=mask,
        detail=keycode,
        time=Xlib.X.CurrentTime,
    )
    window.send_event(event, propagate=True)

if __name__ == "__main__":
    xlib_display = get_xlib_display()
    xwindow = xlib_display.create_resource_object("window", 73400407)

    # send_string(xwindow, "x", ["Ctrl"], False)
    # send_string(xwindow, "x", ["Ctrl"], True)

    # send_string(xwindow, "h", [], False)
    # send_string(xwindow, "h", [], True)

    send_string(xwindow, "y", ["Super"], False)
    send_string(xwindow, "y", ["Super"], True)

    xlib_display.sync()
