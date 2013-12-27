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

;;; ### Misc ###
;;; --- 一般设置
;; (mouse-avoidance-mode "banish")         ;只要一操作鼠标自动闪开

;;; ### Coding ###
;;; --- 编码设置
(setq default-buffer-file-coding-system 'utf-8-unix)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8-unix)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8-unix)               ;键盘输入编码
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)) ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8-unix)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8-unix)               ;终端编码

;;; ### Advice ###
;;; --- 各种emacs行为建议
;; 在特定地模式下粘贴时自动缩进
(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode '(emacs-lisp-mode
                           scheme-mode
                           lisp-mode
                           lisp-interaction-mode
                           c-mode
                           c++-mode
                           objc-mode
                           latex-mode
                           plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;; ### Mode-line ###
;;; --- mode-line

;;; ### Scroll-mode-line ###
;;; --- 滚动 Mode-line 的信息

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

;;; ### Tabbar ###
;;; --- 多标签浏览

;;; ### Files ###
;;; --- 文件设置

;;; ### Browse-kill-ring ###
;;; --- 浏览删除环
(browse-kill-ring-default-keybindings)  ;加载默认的按键邦定

;;; ### Recentf ###
;;; --- 打开最近的文件
(recentf-mode 1)
(setq recentf-max-saved-items 100)      ;最近打开文件的最大数量
(setq recentf-auto-cleanup 300)         ;自动清理最近打开文件列表中重复或其他文件的时间间隔 (秒)
(setq recentf-save-file "~/.emacs.d/deepin-emacs/Configure-File/Recentf/recentf-list") ;最近打开的文件列表

;;; ### Uniquify ###
;;; --- 相同缓存名字时加上路径以区别

;;; ### Miniedit ###
;;; --- 编辑 Minibuffer

;;; ### Cycle-buffer ###
;;; --- 循环切换 Buffer
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if thisc buffer will be considered." t)

;;; ### auto-mode-alist ###
;;; --- 绑定扩展名到特定的模式
(dolist (elt-cons '(
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.coffee$" . coffee-mode)
                    ("\\.iced$" . coffee-mode)
                    ("Cakefile" . coffee-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.[hg]s\\'" . haskell-mode)
                    ("\\.hi\\'" . haskell-mode)
                    ("\\.hs-boot\\'" . haskell-mode)
                    ("\\.chs\\'" . haskell-mode)
                    ("\\.l[hg]s\\'" . literate-haskell-mode)
                    ("\\.inc\\'" . asm-mode)
                    ("\\.max\\'" . maxima-mode)
                    ("\\.lrc\\'" . emms-lyrics-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.a90\\'" . intel-hex-mode)
                    ("\\.hex\\'" . intel-hex-mode)
                    ("\\.html\\'" . html-helper-mode)
                    ("SConstruct". python-mode)
                    ("\\.ml\\'" . tuareg-mode)
                    ("\\.mli\\'" . tuareg-mode)
                    ("\\.mly\\'" . tuareg-mode)
                    ("\\.mll\\'" . tuareg-mode)
                    ("\\.mlp\\'" . tuareg-mode)
                    ("\\.qml\\'" . qml-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

;; Zencoding mode.
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-helper-mode-hook 'zencoding-mode)
(setq zencoding-preview-default nil)

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

;;; ### Speedbar ###
;;; --- 资源管理器
(setq speedbar-show-unknown-files t)    ;显示文件

;;; ### sr-speedbar ###
;;; --- Same Frame Speedbar
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-right-side nil)

;;; ### Highlight-parentheses ###
;;; --- 增强的括号高亮
(add-hook 'find-file-hook 'highlight-parentheses-mode t)

;;; ### Kill ring search ###
;;; --- 删除环搜索
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))

;;; ### Modeline-posn-column-limit ###
;;; --- 列数限制
(setq modelinepos-column-limit 80)      ;设置列数限制, 并在mode-line上显示

;;; ### Lisp install automatically ###
;;; --- Lisp 文件自动安装
(setq install-elisp-repository-directory "/usr/share/deepin-emacs/Site-Lisp/Packages/LazyCatCollect/") ;elisp自动安装目录
(setq install-elisp-confirm-flag nil)     ;不确认安装
(setq install-elisp-use-url-retrieve nil) ;用外部程序下载

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

;;; ### find-func ###
;;; --- 查找函数
(setq find-function-C-source-directory "/usr/share/deepin-emacs/Src") ;设置Emacs的C语言代码目录

;;; ### hl-line ###
;;; --- 高亮当前行

;;; ### timid ###
;;; --- timid补全
(timid-mode 1)
(timid-iswitchb-setup)

;;; ### Google-client ###
;;; --- Google 客户端
(setq g-user-email my-mail)             ;邮件地址

;;; ### Doc View ###
;;; --- PDF, PS, DVI 图书浏览器
(setq doc-view-cache-directory my-translate-png-directory) ;doc-view转换的图书目录
(setq doc-view-image-width (- (display-pixel-width) 16))
(setq doc-view-resolution 300)

;;; ### Auto-fill ###
;;; --- 自动换行
(setq default-fill-column 100)          ;默认显示 100列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               'org-mode-hook
               ))
  (add-hook hook '(lambda () (auto-fill-mode 1))))

;;; ### Tempbuf ###
;;; --- 临时Buffer管理
(setq tempbuf-kill-message nil)         ;不在Mode-line显示删除临时buffer提示消息
(setq tempbuf-minimum-timeout 30)       ;删除 buffer 的最低期限
(dolist (hook (list
               'compilation-mode-hook     ;编译模式
               'comint-mode-hook          ;comint 模式
               'completion-list-mode-hook ;补全列表模式
               'help-mode-hook            ;帮助模式
               'Info-mode-hook            ;Info 模式
               'calc-mode-hook            ;计算器模式
               'gnus-article-mode-hook    ;Gnus 文章模式
               'gnus-kill-file-mode       ;Gnus 删除文件模糊
               ))
  (add-hook hook 'turn-on-tempbuf-mode)) ;加载自动清理临时buffer

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

;;; ### Term ###
;;; --- 终端模拟器
(setq term-eol-on-send t)               ;输入前跳转到最后一行

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
(setq isearch-allow-scroll t)           ;isearch搜索时是可以滚动屏幕的

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

;;; ### Auto-Install ###
;;; --- 自动下载安装代码
(setq auto-install-directory "/usr/share/deepin-emacs/Site-Lisp/Packages/LazyCatCollect/") ;设置默认的安装目录
(setq auto-install-from-w3m-confirm nil) ;从w3m安装不提醒

;;; ### Winner-mode ###
;;; --- 窗口设置的撤销和返回
(winner-mode 1)

;;; ### one-key ###
;;; --- one key
(setq one-key-popup-window nil)         ;禁止自动弹出窗口

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

;;; ### Irfc ###
;;; --- RFC 文档阅读
(setq irfc-directory "/data/Book/Network_Programming/RFC-all") ;设置存储目录
(custom-set-variables                                          ;自动关联 `irfc-mode'
 '(irfc-assoc-mode t))

;;; ### Auto-Install ###
;;; --- 自动安装elisp
(setq auto-install-save-confirm nil)    ;不需要确认保存

;;; ### Yaoddmuse ###
;;; --- 编辑 Wiki
(setq yaoddmuse-browse-function 'yaoddmuse-browse-page-in-w3m)  ;设置yaoddmuse浏览函数
(setq yaoddmuse-notify-function 'yaoddmuse-notify-popup-window) ;设置yaoddmuse提示函数
(setq yaoddmuse-wikis                                           ;只更新 EmacsWiki
      '(("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8 "uihnscuskc=1;")))

;;; ### highlight-cl ###
;;; --- 高亮 `cl' 函数
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)

;;; ### apt-utils ###
;;; --- apt 工具
(setq apt-utils-automatic-update t)     ;总是自动重建包列表， 不用询问

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

;; ### nxhtml-mode ###
;;; --- nxhtml mode.
;; (load "/home/andy/MyEmacs/Site-Lisp/Packages/nxhtml/autostart.el")

;; ### Pretty lambda ###
;; --- Pretty lambda
(add-hook 'python-mode-hook 'pretty-lambda)

;; ### Smooth scrolling ###
;; --- Smooth scrolling
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

(add-hook 'js-mode-hook
          (lambda () (flymake-mode 1)))

;; Asm mode.
(add-hook 'asm-mode-hook
          (lambda () (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))

;; Slime mode.
(setq inferior-lisp-program "/usr/bin/sbcl")
(slime-setup '(slime-fancy))

(defun remove-python-comment()
  (interactive)
  (goto-char (point-min))
  (goto-line 3)
  (while (< (point) (point-max))
    (progn
      (comment-indent)
      (unless (elisp-format-in-string-p)
        (if (elisp-format-in-comment-p)
            (progn
              (delete-region (point) (save-excursion
                                       (end-of-line)
                                       (point)))
              (backward-delete-char 2))
          ))
      (forward-line 1)
      )))

(provide 'init-misc)

;;; init-misc.el ends here
