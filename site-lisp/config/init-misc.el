;;; init-misc.el --- Misc configuration with emacs

;; Filename: init-misc.el
;; Description: Misc configuration with emacs
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:07:08
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:07:12
;;           By: Andy Stewart
;; URL:
;; Keywords: misc
;; Compatibility: GNU Emacs 23.0.60.1
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
;; Misc configuration with emacs
;;

;;; Installation:
;;
;; Put init-misc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-misc)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
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

;;; ### Ctypes-auto-parse ###
;;; --- 自动对 C 语言的类型定义识别并进行语法加亮
(ctypes-auto-parse-mode 1)

;;; ### Modeline-posn ###
;;; --- 在 Mode-line 显示当前Buffer的大小
(size-indication-mode 1)

;;; ### Hanconvert ###
;;; --- 自动在简体中文和繁体中文间转换.
(autoload 'hanconvert-region "hanconvert"
  "Convert a region from simple chinese to tradition chinese or
from tradition chinese to simple chinese" t)

;;; ### Browse-kill-ring ###
;;; --- 浏览删除环
(browse-kill-ring-default-keybindings)  ;加载默认的按键邦定

;;; ### Recentf ###
;;; --- 打开最近的文件
(recentf-mode 1)
(setq recentf-max-saved-items 100)      ;最近打开文件的最大数量
(setq recentf-auto-cleanup 300)         ;自动清理最近打开文件列表中重复或其他文件的时间间隔 (秒)
(setq recentf-save-file "~/.emacs.d/deepin-emacs/Configure-File/Recentf/recentf-list") ;最近打开的文件列表

;;; ### Doxymacs ###
;;; --- 注释管理
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               ))
  (doxymacs-font-lock)                                                        ;注释高亮模式
  (add-hook hook 'doxymacs-mode)                                              ;加载文档模式
  (add-hook hook (lambda () (local-set-key (kbd "C-m") 'my-doxymacs-return))) ;注释智能换行
  )

;;; ### Whitespace ###
;;; --- 空格清理
(setq whitespace-global-mode t)         ;全局检测多余空格

;;; ### Mail-notify ###
;;; --- 邮件提醒
(setq mail-notify-directory "~/.emacs.d/deepin-emacs/Mail/inbox/new/") ;新邮件存放目录
(setq mail-notify-status t)                                            ;默认打开邮件提醒
(setq mail-notify-repeat 60)                                           ;邮件提醒的周期 (秒)

;;; ### Save-abbreviation ###
;;; --- Elisp 命令别名
(setq save-abbreviation-file "~/.emacs.d/deepin-emacs/Configure-File/Save-Abbreviation/abbreviation") ;Lisp别名存储的位置
(save-abbreviation-mode)

;;; ### Ibuffer ###
;;; --- 交互式Buffer
(setq ibuffer-sorting-mode 'recency)    ;用最近打开模式显示

;;; ### Contentswitch ###
;;; --- 根据内容进行Buffer和File切换
(setq contentswitch-file-completion-delay 0.1) ;匹配显示延迟
(setq contentswitch-max-name-length 40)        ;名字显示的最大宽度

;;; ### timid ###
;;; --- timid补全
(timid-mode 1)
(timid-iswitchb-setup)

;;; ### Google-client ###
;;; --- Google 客户端
(setq g-user-email my-mail)             ;邮件地址

;;; ### Breadcrumb ###
;;; --- 文件书签管理
(setq bc-bookmark-file "~/.emacs.d/deepin-emacs/Configure-File/Breadcrumb/bookmark") ;设置书签的保存位置
(setq bc-bookmark-limit 300)            ;设置书签的最大数量

;;; ### Tramp ###
;;; --- 多协议远程访问
(setq tramp-default-method "ssh")         ;设置传送文件默认的方法
(custom-set-variables '(tramp-verbose 0)) ;设置tramp的响应方式, 关闭后不弹出消息

;;; ### Calendar ###
;;; --- 日历
(setq calendar-week-start-day 1)                ;星期一作为一周开始的第一天
(setq calendar-chinese-location-name "Chengdu") ;本地名称
(setq calendar-date-style (quote iso))          ;日期格式种类
(setq diary-file "~/Diary/diary")               ;日记文件

;;; ### Traverse ###
;;; --- 在目录中搜索和替换, 支持AVFS (虚拟文件系统, 支持压缩文件直读)
(setq traverse-use-avfs t)                           ;开启AVFS
(add-to-list 'traverse-ignore-files ".ledger-cache") ;忽略的文件

;;; ### Xgtags ###
;;; --- Gtags 的界面
(add-hook 'c-mode-common-hook 'xgtags-mode)

;;; ### Browse-kill-ring ###
;;; --- 浏览删除环
(setq browse-kill-ring-quit-action      ;设置退出动作
      (quote save-and-restore))         ;保存还原窗口设置

;;; ### Window-number ###
;;; --- 窗口数字导航
(window-number-mode 1)                  ;开启

;;; ### cursor-chg ###
;;; --- 光标随着状态改变形状和颜色

;;; ### Byte-compile-warnings ###
;;; --- 编译时显示的警告设置
(setq byte-compile-warnings
      (quote (
              ;; 显示的警告
              free-vars                 ;不在当前范围的引用变量
              unresolved                ;不知道的函数
              callargs                  ;函数调用的参数和定义的不匹配
              obsolete                  ;荒废的变量和函数
              noruntime                 ;函数没有定义在运行时期
              interactive-only          ;正常不被调用的命令
              make-local                ;调用 `make-variable-buffer-local' 可能会不正确的
              mapcar                    ;`mapcar' 调用
              ;; 抑制的警告
              (not redefine)            ;重新定义的函数 (比如参数数量改变)
              (not cl-functions)        ;`CL' 包中的运行时调用的函数
              )))

;;; ### C-mode ###
;;; --- C 语言模式
(setq c-echo-syntactic-information-p nil) ;禁止显示回显信息如果代码缩进

;;; ### Ediff ###
;;; --- Emacs 比较文件

;;; ### Icomplet-mode ###
;;; --- minibuffer 不全增量回馈
(icomplete-mode 1)

;;; ### Isearch ###
;;; --- 增量搜索

;;; ### Cldoc ###
;;; --- Common Lisp 的操作符和变量的信息提示
(add-hook 'lisp-mode-hook 'turn-on-cldoc-mode)

;;; ### Imenu ###
;;; --- buffer 索引
(setq imenu-sort-function 'imenu--sort-by-name) ;按名字排序
(dolist (hook (list
               'c-mode-common-hook
               'emacs-lisp-mode-hook
               'lisp-mode-hook
               'lisp-interaction-mode-hook
               ))
  (add-hook hook 'imenu-add-menubar-index))

;;; ### Woman ###
;;; --- 手册查询
(setq woman-default-indent 7            ;缩进格式
      woman-fill-frame t                ;填充满屏幕
      woman-use-own-frame nil           ;同一个frame
      woman-cache-level 3)              ;缓存级别, 最快

;;; ### File-Journal ###
;;; ---
(setq fj-journal-size 10)               ;设置保留文件的天数
(setq fj-journal-file                   ;设置配置文件的保留位置
      "~/.emacs.d/deepin-emacs/Configure-File/File-Journal/file-journal")

;;; ### babel ###
;;; --- 网络翻译接口
(setq babel-preferred-from-language "English")            ;首选的翻译语言
(setq babel-preferred-to-language "Chinese (Simplified)") ;首选的目标语言

;;; ### Paste2 ###
;;; --- paste2.org 的粘贴服务
(setq paste2-user erc-nick)             ;默认的用户名

;;; ### Winner-mode ###
;;; --- 窗口设置的撤销和返回
(winner-mode 1)

;;; ### elisp-depend ###
;;; --- 分析elisp文件依赖
(setq elisp-depend-directory-list       ;设置忽略的检查目录
      '("/usr/share/deepin-emacs/Common/share/emacs/"
        "/usr/share/deepin-emacs/Site-Lisp/Configure/"))

;;; ### Winpoint ###
;;; --- 记住每一个窗口 buffer 的位置
(window-point-remember-mode 1)
(setq winpoint-non-restore-buffer-list
      '("*Group*"))

;;; ### highlight-cl ###
;;; --- 高亮 `cl' 函数
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)

;;; ### apt-utils ###
;;; --- apt 工具

;;; ### Compilation ###
;;; --- 编译选项
(compilation-always-kill-mode 1)
(add-hook 'compilation-mode-hook 'compilation-recenter-end-enable)

;; ### Android ###
;;; --- Android mode.
(require 'android-mode)
(setq android-mode-sdk-dir "/usr/local/android-sdk-linux_86/")
(setq android-mode-avd "Android-2.2")
(setq java-mode-hook
      (function (lambda()
                  (java-mode-indent-annotations-setup))))

;; ### Local variable ###
;; --- Turn off file variables.
(setq enable-local-variables nil
      enable-local-eval nil)

(add-hook 'js-mode-hook
          (lambda () (flymake-mode 1)))

;; Asm mode.
(add-hook 'asm-mode-hook
          (lambda () (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))

;; Slime mode.
(setq inferior-lisp-program "/usr/bin/sbcl")
(slime-setup '(slime-fancy))

(provide 'init-misc)

;;; init-misc.el ends here
