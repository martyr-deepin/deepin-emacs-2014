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
import time
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
    if keycode == 0:
        print "Sorry, can't map", ch

    if is_shifted(ch):
        shift_mask = Xlib.X.ShiftMask
    else:
        shift_mask = 0

    return keycode, shift_mask

def send_string(window, str):
    xlib_display = get_xlib_display()
    
    for ch in str:
        keycode, shift_mask = char_to_keycode(ch)
        if keycode == 0:
            keycode, shift_mask = char_to_keycode('_')

        print 'sending [{0!r}] keycode={1} with shift_mask={2}'.format(
            ch, keycode, shift_mask)
        for eventtype in (Xlib.protocol.event.KeyPress,
                          Xlib.protocol.event.KeyRelease):
            event = eventtype(root=xlib_display.screen().root,
                              window=window,
                              same_screen=0,
                              child=Xlib.X.NONE,
                              root_x=0, root_y=0,
                              event_x=0, event_y=0,
                              state=shift_mask,
                              detail=keycode,
                              time=int(time.time()))
            window.send_event(event, propagate=True)
