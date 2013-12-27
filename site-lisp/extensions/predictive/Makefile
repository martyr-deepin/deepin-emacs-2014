
# Copyright (C) 2004-2008 Toby Cubitt

# Author: Toby Cubitt <toby-predictive@dr-qubit.org>
# Version: 0.5
# URL: http://www.dr-qubit.org/emacs.php

# This file is part of the Emacs Predictive Completion package.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.


EMACS = emacs
DESTDIR = ~/.emacs.d/predictive
DICTDIR = ~/.emacs.d/predictive
INFODIR = /usr/share/info


all: core dict-english dicts info


docs: info pdf dvi txt ps html


clean:
	find . -name '*.elc' -delete
	[ ! -e dict-english.el ] || rm dict-english.el
	[ ! -e predictive-user-guide.info.gz ] || rm predictive-user-guide.info.gz
	[ ! -e predictive-user-guide.pdf ] || rm predictive-user-guide.pdf
	[ ! -e predictive-user-guide.dvi ] || rm predictive-user-guide.dvi
	[ ! -e predictive-user-guide.ps.gz ] || rm predictive-user-guide.ps.gz
	[ ! -e predictive-user-guide.txt.gz ] || rm predictive-user-guide.txt.gz
	[ ! -d predictive-user-guide-html ] || rm -r predictive-user-guide-html/


.PHONY : install

install: all
	mkdir -p $(DESTDIR)
	find . \( -name '*.el' -o -name '*.elc' \) -a \( -name dict-tree.el -o -name dict-tree.elc -o \! -name 'dict-*' \) -execdir cp {} $(DESTDIR) \;
	mkdir -p $(DICTDIR)
	cp dict-english.el dict-english.elc $(DICTDIR)
	for d in `find . -mindepth 1 -type d -exec mkdir -p $(DICTDIR)/{} \; -print`; do cp $$d/dict-*.el $$d/dict-*.elc $(DICTDIR)/$$d; done

	@echo
	@echo "To complete the installation, add the following lines to your .emacs file:"
	@echo
	@echo "  (add-to-list 'load-path \"$(DESTDIR)\")"
	@[ "$(DICTDIR)" = "$(DESTDIR)" ] || echo "  (add-to-list 'load-path \"$(DICTDIR)\")"
	@for d in `find . -mindepth 1 -type d`; do echo "  (add-to-list 'load-path \"$(DICTDIR)/$${d#./}\")"; done
	@echo "  (require 'predictive)"
	@echo




# list of core elisp files
core_files := $(shell ls *.el | grep -v 'dict-english.el' | sed 's:\.el:\.elc:g')

# list of libraries to load
#elisp_libs = heap.el tstree.el dict.el predictive.el auto-overlays.el auto-overlay-word.el auto-overlay-line.el auto-overlay-self.el auto-overlay-stack.el

# lists of dictionaries
latex_dicts := $(shell ls latex/dict-*.word-list | sed 's:\.word-list:\.elc:g')
html_dicts := $(shell ls html/dict-*.word-list | sed 's:\.word-list:\.elc:g')
texinfo_dicts := $(shell ls texinfo/dict-*.word-list | sed 's:\.word-list:\.elc:g')
#f90_dicts := $(shell ls f90/dict-*.word-list | sed 's:\.word-list:\.elc:g')




# byte-compilation target
core: $(core_files)

# overrides implicit rules, since these require the dictionaries
predictive-latex.elc: predictive-latex.el $(latex-dicts)
	$(EMACS) --batch -L ./ -L ./latex/ -f batch-byte-compile $<
predictive-html.elc: predictive-html.el $(html-dicts)
	$(EMACS) --batch -L ./ -L ./html/ -f batch-byte-compile $<




# English dictionary target
dict-english: dict-english.elc

# overrides implicit rule for dictionaries, to create it from the .el file
dict-english.elc: dict-english.el
	$(EMACS) --batch -L ./ -f batch-byte-compile $<

# in case dict-english.el doesn't exist (should be included in package)
dict-english.el: dict-english.word-list dict-tree.elc
	$(EMACS) --batch -L ./ --eval="(progn (require 'predictive) (setq dict-english (predictive-create-dict '$(basename $(notdir $@)) \"$(basename $@)\" \"$<\")) (dictree-write dict-english \"dict-english\" t))"




# dictionary targets
dicts: dict-english latex_dicts html_dicts texinfo_dicts # f90_dicts

latex_dicts: $(latex_dicts)

html_dicts: $(html_dicts)

texinfo_dicts: $(texinfo_dicts)

#f90_dicts: $(f90_dicts)




# documentation targets
info: predictive-user-guide.info.gz

pdf: predictive-user-guide.pdf

dvi: predictive-user-guide.dvi

ps: predictive-user-guide.ps.gz

txt: predictive-user-guide.txt.gz

html: predictive-user-guide-html


# info file installation target
install-info: predictive-user-guide.info.gz
	install-info predictive-user-guide.info.gz $(INFODIR)/dir




# implicit rule for creating dictionaries
dict-%.elc: dict-%.word-list dict-tree.elc
	$(EMACS) --batch -L ./ --eval="(progn (require 'predictive) (predictive-create-dict '$(basename $(notdir $@)) \"$(basename $@)\" \"$<\") (dictree-save-modified))"


# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch -L ./ -f batch-byte-compile $<


# implicit rules for making docs in various formats
%.info: %.texinfo
	makeinfo $< -o $@

%.info.gz: %.info
	gzip -f $< -c > $@

%.txt: %.texinfo
	makeinfo --plaintext $< > $@

%.txt.gz: %.texinfo
	makeinfo --plaintext $< | gzip -c > $@

%.dvi: %.texinfo
	texi2dvi -c -o $@ $<

%.pdf: %.texinfo
	texi2dvi --pdf -c -o $@ $<

%.ps.gz: %.dvi
	dvips -f $< | gzip -c > $@

%-html: %.texinfo
	makeinfo --html $< -o $(dir $@)/html

%-html.tar.gz: %-html
	cd $(dir $@); pwd; tar -cvzf $(notdir $@) predictive-user-guide-html
