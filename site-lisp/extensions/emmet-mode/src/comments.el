;;; emmet-mode.el --- Unofficial Emmet's support for emacs

;; Copyright (C) 2013-     Shin Aoyama
;; Copyright (C) 2009-2012 Chris Done

;; Version: 1.0.5
;; Author: Shin Aoyama <smihica@gmail.com>
;; URL: https://github.com/smihica/emmet
;; Last-Updated: 2013-09-16 Mon
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Unfold CSS-selector-like expressions to markup. Intended to be used
;; with sgml-like languages; xml, html, xhtml, xsl, etc.
;;
;; See `emmet-mode' for more information.
;;
;; Copy emmet-mode.el to your load-path and add to your .emacs:
;;
;;    (require 'emmet-mode)
;;
;; Example setup:
;;
;;    (add-to-list 'load-path "~/Emacs/emmet/")
;;    (require 'emmet-mode)
;;    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;;    (add-hook 'html-mode-hook 'emmet-mode)
;;    (add-hook 'css-mode-hook  'emmet-mode)
;;
;; Enable the minor mode with M-x emmet-mode.
;;
;; See ``Test cases'' section for a complete set of expression types.
;;
;; If you are hacking on this project, eval (emmet-test-cases) to
;; ensure that your changes have not broken anything. Feel free to add
;; new test cases if you add new features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; This is a fork of zencoding-mode to support Emmet's feature.
;; zencoding-mode (https://github.com/rooney/zencoding)


