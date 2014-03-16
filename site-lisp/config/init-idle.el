;;; init-idle.el --- Configure file that can load when emacs idle.

;; Filename: init-idle.el
;; Description: Configure file that can load when emacs idle.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-17 03:06:11
;; Version: 0.1
;; Last-Updated: 2014-03-17 03:06:11
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-idle.el
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
;; Configure file that can load when emacs idle.
;;

;;; Installation:
;;
;; Put init-idle.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-idle)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-idle RET
;;

;;; Change log:
;;
;; 2014/03/17
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

(pretty-lambda-for-modes)
(browse-kill-ring-default-keybindings)    ;加载默认的按键邦定
(setq browse-kill-ring-quit-action        ;设置退出动作
      (quote save-and-restore))           ;保存还原窗口设置
(autoload 'hanconvert-region "hanconvert" ;简繁中文互相转换
  "Convert a region from simple chinese to tradition chinese or
from tradition chinese to simple chinese" t)
(autoload 'irfc "init-irfc")
(autoload 'doc-view-mode "init-doc-view")
(custom-set-variables '(tramp-verbose 0)) ;设置tramp的响应方式, 关闭后不弹出消息
(setq max-lisp-eval-depth 40000)        ;lisp最大执行深度
(setq max-specpdl-size 10000)           ;最大容量
(setq kill-ring-max 1024)               ;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq mark-ring-max 1024)               ;设置的mark ring容量
(setq eval-expression-print-length nil) ;设置执行表达式的长度没有限制
(setq eval-expression-print-level nil)  ;设置执行表达式的深度没有限制
(auto-compression-mode 1)               ;打开压缩文件时自动解压缩
(setq read-quoted-char-radix 16)        ;设置 引用字符 的基数
(setq global-mark-ring-max 1024)        ;设置最大的全局标记容量

(provide 'init-idle)

;;; init-idle.el ends here
