;;; init-generic.el --- Generic config

;; Filename: init-generic.el
;; Description: Generic config
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-20 23:57:56
;; Version: 0.1
;; Last-Updated: 2014-01-20 23:57:56
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-generic.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Generic config
;;

;;; Installation:
;;
;; Put init-generic.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-generic)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-generic RET
;;

;;; Change log:
;;
;; 2014/01/20
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:


;; Restore emacs session.
(setq initial-buffer-choice "~")
(run-with-timer 1 nil #'(lambda () (bury-buffer)))
(emacs-session-restore)

(fset 'yes-or-no-p 'y-or-n-p)                            ;以 y/n代表 yes/no
(blink-cursor-mode -1)                                   ;指针不闪动
(transient-mark-mode 1)                                  ;标记高亮
(setq-default comment-style 'indent)                     ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)                        ;关闭烦人的出错时的提示声
(setq default-major-mode 'text-mode)                     ;设置默认地主模式为TEXT模式
(setq mouse-yank-at-point t)                             ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)                       ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)                         ;分屏的时候使用上下分屏
(add-hook 'find-file-hook 'highlight-parentheses-mode t) ;增强的括号高亮

(provide 'init-generic)

;;; init-generic.el ends here
