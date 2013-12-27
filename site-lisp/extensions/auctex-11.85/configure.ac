dnl Autoconf script for AUCTeX

dnl Maintainer: auctex-devel@gnu.org

dnl Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

dnl This file is part of AUCTeX.

dnl AUCTeX is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.

dnl AUCTeX is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with AUCTeX; see the file COPYING.  If not, write to the Free
dnl Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
dnl MA 02110-1301, USA.

AC_INIT(auctex,11.85,bug-auctex@gnu.org)

AC_CHECK_PROGS_REQUIRED(MAKECMD, make, [make not found, aborting!])
AC_PROG_MAKE_SET
AC_PROG_INSTALL

AC_DATE_VERSION_FROM_CHANGELOG(AUCTEXDATE,AUCTEXVERSION)
AC_SUBST(AUCTEXDATE)
AC_SUBST(AUCTEXVERSION)

EMACS_PROG_EMACS

if test ${EMACS_FLAVOR} = xemacs
then
  EMACS_CHECK_VERSION(21,4)
else
  EMACS_CHECK_VERSION(21,1)
fi

EMACS_CHECK_MULE

# The Debian package uses `--disable-build-dir-test'; normal users should
# never need to modify the default behavior.
AC_ARG_ENABLE([build-dir-test],
  [  --enable-build-dir-test test for build directory inside `load-path'
			  (on by default)],
  [if ! [[ "X-${enableval}" = "X-no" ]]
     then VALID_BUILD_DIR
   fi],
  [VALID_BUILD_DIR])

if test ${EMACS_FLAVOR} = "xemacs"  ; then
   EMACS_PATH_PACKAGEDIR
else
   packagedir=no
fi

if test "X${packagedir}" = Xno
then
   EMACS_PATH_LISPDIR
   AC_MSG_CHECKING(what file to use for auctex startup)
   AC_ARG_WITH(auctexstartfile,
   [[  --with-auctexstartfile=FILE    What file to use for auctex startup.]],
   [[ auctexstartfile="${withval}" ]],
   [ _tmpdir_="${lispdir}"
     AC_FULL_EXPAND(_tmpdir_)
     if test -d "${_tmpdir_}/site-start.d"
     then
       auctexstartfile="${lispdir}/site-start.d/auctex.el"
     else
       auctexstartfile="${lispdir}/auctex.el"
     fi])
   AC_MSG_RESULT([[${auctexstartfile}]])
   packagelispdir="${lispdir}/auctex"
   packagedatadir="${packagelispdir}"
   texsitedir="${lispdir}"
else
   #lispdir is only used for determining relative files.
   lispdir="${packagedir}"
   packagelispdir="${packagedir}/lisp/auctex"
   texsitedir="$packagelispdir"
   auctexstartfile="${packagelispdir}"/auto-autoloads.el
   packagedatadir="${packagedir}/etc/auctex"
   infodir="${packagedir}/info"
   INSTALL_INFO=:
fi

AC_SUBST(auctexstartfile)

AC_MSG_CHECKING([where the package lisp files go])
AC_ARG_WITH(packagelispdir,
[[  --with-packagelispdir=DIR
                        Directory for the package lisp files.]],
  [packagelispdir=["${withval}"]])
AC_LISPIFY_DIR(packagelispdir,[["${texsitedir}/tex-site.el"]])
AC_MSG_RESULT([[${packagelispdir}]])

AC_MSG_CHECKING([where the data files go])
AC_ARG_WITH(packagedatadir,
[[  --with-packagedatadir=DIR         Where the data files go.]],
  [packagedatadir=["${withval}"]])
AC_LISPIFY_DIR(packagedatadir,[["${texsitedir}/tex-site.el"]])
AC_MSG_RESULT([[${packagedatadir}]])

AC_LISPIFY_DIR(texsite,[["${auctexstartfile}"]],tex-site.el)

AC_MSG_CHECKING(where the info files go)
AC_MSG_RESULT([[${infodir}]])

AC_SUBST(packagedir)

if test $EMACS_FLAVOR = "xemacs" -a "$infodir" = '${prefix}/info'; then
  if test $packagedir != 'no'; then
   infodir='${packagedir}/info'
  fi
fi

AUCTEX_AUTO_DIR
AC_LISPIFY_DIR(autodir,"${texsitedir}/tex-site.el")

AC_PATH_PROG(TEX, tex, :)
AC_PATH_PROG(PDFTEX, pdftex, :)
AC_PATH_PROG(DVIPS, dvips, :)

AC_ARG_VAR(PERL,[Perl executable.  If set to :, some documentation
cannot be regenerated, but installation of an unmodified tarball will
succeed.])

if test "x$PERL" = x
then
   AC_PATH_PROG(PERL, perl, :)
fi

AC_ARG_VAR(MAKEINFO,[Makeinfo executable.  If set to :, some documentation
cannot be regenerated, but installation of an unmodified tarball will
succeed.])

AC_PATH_PROG(MAKEINFO, makeinfo, :)
AC_PATH_PROG(TEXI2HTML, texi2html, :)
AC_PATH_PROG(TEXI2DVI, texi2dvi, :)
AC_PATH_PROG(TEXI2PDF, texi2pdf, :)

AC_ARG_VAR(INSTALL_INFO, [install-info executable.  Set to : to skip making a dir file.  This is the default when installing into an XEmacs package.])

if test "X${INSTALL_INFO}" = X
then
  AC_PATH_PROG(INSTALL_INFO, install-info, :, ${PATH} /usr/sbin /sbin)
fi

AC_ARG_ENABLE(preview,
  [  --disable-preview       disable inclusion of preview-latex],
  [preview_enabled="$enableval"],
  [preview_enabled="yes"])
AC_SUBST(preview_enabled)

if test "X${preview_enabled}" = Xyes
then
  auctexdir=..; export auctexdir
  export PDFTEX
  export DVIPS
  # Export unquoted variables.
  TEX_UNQUOTED="$TEX"
  export TEX_UNQUOTED
  packagedir_unquoted="$packagedir"
  export packagedir_unquoted
  packagelispdir_unquoted="$packagelispdir"
  export packagelispdir_unquoted
  packagedatadir_unquoted="$packagedatadir"
  export packagedatadir_unquoted
  EMACS_UNQUOTED="$EMACS"
  export EMACS_UNQUOTED
  AC_CONFIG_SUBDIRS(preview)
fi
AC_SUBST(subdirs)

AC_SHELL_QUOTIFY(prefix)
AC_SHELL_QUOTIFY(localstatedir)
AC_SHELL_QUOTIFY(packagedir)
AC_SHELL_QUOTIFY(packagelispdir)
AC_SHELL_QUOTIFY(packagedatadir)
AC_SHELL_QUOTIFY(lispdir)
AC_SHELL_QUOTIFY(infodir)
AC_SHELL_QUOTIFY(datadir)
AC_SHELL_QUOTIFY(autodir)
AC_SHELL_QUOTIFY(auctexstartfile)
AC_SHELL_QUOTIFY(EMACS)
AC_SHELL_QUOTIFY(INSTALL_INFO)
AC_SHELL_QUOTIFY(PERL)
AC_SHELL_QUOTIFY(MAKEINFO)
AC_SHELL_QUOTIFY(TEX)
AC_SHELL_QUOTIFY(PDFTEX)
AC_SHELL_QUOTIFY(DVIPS)
AC_SHELL_QUOTIFY(TEXI2HTML)
AC_SHELL_QUOTIFY(TEXI2DVI)
AC_SHELL_QUOTIFY(TEXI2PDF)

AC_OUTPUT(Makefile tex-site.el.out:tex-site.el.in doc/Makefile auctex.el)


cat >&2 <<EOF

Congratulations!  Configuration of AUCTeX is complete.

Type "make" at the prompt to build.
EOF
