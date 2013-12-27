# Copyright (C) 2005 Ivan Kanis
# Author: Ivan Kanis
# $Id$
#
# This program is free software ; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation ; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY ; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program ; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

# build lookup

# ndnmz.el  requires flim

SRC = \
mldonkey-auth.el \
mldonkey-commands.el \
mldonkey-console.el \
mldonkey.el \
mldonkey-header-line.el \
mldonkey-vd.el \
mldonkey-vd-sort.el

ELC = ${SRC:.el=.elc}

AUTOFILE = mldonkey-auto-load

INSTALLDIR = ${HOME}/build/lisp/ivan

# remove warnings
EMACSARG = --eval '(setq byte-compile-warnings nil)'

all: $(ELC) $(AUTOFILE).elc

$(AUTOFILE).el: $(SRC)
	  @$(EMACS) $(AUTOLOAD) $(SRC) 

include ../elisp.mk
