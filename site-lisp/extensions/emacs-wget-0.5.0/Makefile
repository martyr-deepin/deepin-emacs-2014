############################################################
#                  Emacs-wget Makefile                     #
############################################################

## Copyright (C) 2002, 2003, 2004  Masayuki Ataka  <ataka@milk.freemail.ne.jp>
#  $Id: Makefile,v 1.13 2004/10/19 07:33:08 ataka Exp $
#

PACKAGE = emacs-wget
VERSION = 0.5.0

prefix = /usr/local
datadir = ${prefix}/share
lispdir = ${datadir}/emacs/site-lisp/emacs-wget

DOC = COPYING ChangeLog Makefile README README.ja USAGE USAGE.ja
EL  = wget.el wget-sysdep.el w3-wget.el w3m-wget.el
ELC = wget.elc wget-sysdep.elc w3-wget.elc w3m-wget.elc
LPATH = lpath.el

MKDIR = mkdir
TAR   = tar
CP    = cp -p
RM    = rm -f
EMACS = emacs

ELCC  = $(EMACS) -batch -q -no-site-file -l $(LPATH)

.PHONY : all compile-elc
all: compile-elc

compile-elc: $(ELC)


%.elc: %.el
	$(ELCC) -f batch-byte-compile $<

## Install emacs-wget
#
.PHONY : install
install:
	if [ ! -d $(lispdir) ]; then \
	  $(MKDIR) -p $(lispdir); \
	fi
	$(CP) $(EL) $(ELC) $(lispdir)

## Release new version
#
# Set tag name, before make tar ball:
#  $ cvs tag -c rel-X-X-X
#
# Checkout any release of emacs-wget:
#  $ cvs checkout -r rel-X-X-X emacs-wget
#
# Remove tag name:
#  $ cvs tag -d rel-X-X-X emacs-wget
#
DISTRIB = $(PACKAGE)-$(VERSION)
dist: distclean
	$(MKDIR) $(DISTRIB)
	$(CP) $(DOC) $(EL) $(LPATH) $(DISTRIB)
	$(TAR) czvf $(DISTRIB).tar.gz $(DISTRIB)

## Clean
#
clean:
	$(RM) *.elc *~
distclean: distclean
	$(RM) -r $(DISTDIR)
	$(RM) $(DISTDIR).tar.gz

#
## End of Makefile
