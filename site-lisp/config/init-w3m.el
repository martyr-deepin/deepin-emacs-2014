;;; init-w3m.el --- W3m configuration

;; Filename: init-w3m.el
;; Description: W3m configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:35:25
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:35:35
;;           By: Andy Stewart
;; URL:
;; Keywords: w3m
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
;; W3m configuration
;;

;;; Installation:
;;
;; Put init-w3m.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-w3m)
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

(require 'w3m)                         ;W3M
(require 'w3m-search)                  ;w3m搜索
(require 'w3m-bookmark)                ;w3m书签
(require 'w3m-tabmenu)                 ;W3m标签菜单
(require 'w3m-wget)                    ;W3m wget 集成
(require 'w3m-lnum)                    ;W3M 数字快速导航
(require 'w3m-form)                    ;W3m 表格
(require 'w3m-util)                    ;w3m 工具

;;; Code:

;; 常规设置
(setq browse-url-browser-function 'w3m-browse-url)      ;设定用Emacs－wiki打开URL
(setq browse-url-new-window-flag t)                     ;用适当的浏览器打开新窗口
(setq w3m-home-page "http://www.google.com")		;设置首页
(setq w3m-make-new-session t)                           ;当浏览一个新的页面时开启一个新的缓存页面
(setq w3m-use-cookies t)                                ;启用cookie
(setq w3m-use-header-line-title t)                      ;显示标题
(setq w3m-cookie-accept-bad-cookies t)                  ;接收 BAD cookie
(setq w3m-view-this-url-new-session-in-background t)    ;后台打开连接
(setq w3m-new-session-in-background t)                  ;后台建立新任务
(setq w3m-session-time-format "%Y-%m-%d %A %H:%M")      ;上次浏览记录的时间显示格式
(setq w3m-favicon-use-cache-file t)                     ;使用网站图标的缓存文件
(setq w3m-show-graphic-icons-in-mode-line nil)          ;不在mode-line显示网站图标
(setq w3m-keep-arrived-urls 50000)                      ;浏览历史记录的最大值
(setq w3m-keep-cache-size 1000)                         ;缓存的大小
(setq w3m-edit-function (quote find-file-other-window)) ;在其他窗口编辑当前页面
(setq w3m-session-automatic-save t)                     ;退出时自动保存
(setq w3m-session-deleted-save nil)                     ;关闭一个标签时不保存
(setq w3m-default-display-inline-images nil)            ;默认不显示网页中的图像
(setq w3m-toggle-inline-images-permanently t)           ;继续保持当前buffer的图像状态
(setq w3m-enable-google-feeling-lucky nil)              ;禁止使用 Google Lucky
(setq w3m-use-filter t)                                 ;开启过滤
(setq w3m-filter-google-separator "")                   ;设置过滤器的分离字符
(setq w3m-fb-mode t)                                    ;让标签和创建它的FRAME关联
(setq w3m-session-load-crashed-sessions t)              ;默认加载崩溃的对话
(w3m-fb-mode 1)                                         ;可以显示FRAME

;; 存储设置
(setq w3m-default-save-directory "~/.emacs.d/download")             ;设置默认的保存目录
(setq w3m-bookmark-file "~/.emacs.d/deepin-emacs/Configure-File/Emacs-W3M/bookmark.html") ;设定书签文件
(setq w3m-cookie-file "~/.emacs.d/deepin-emacs/Configure-File/Emacs-W3M/W3m-cookie")      ;设置w3m-cookie保存位置
(setq w3m-session-file "~/.emacs.d/deepin-emacs/Configure-File/Emacs-W3M/W3m-session")    ;设定任务保存的文件位置
;; 修改Buffer名字为网页的标题或链接, 以利于 anything 搜索
(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*"
                     (prettyfy-string (or w3m-current-title
                                          w3m-current-url) 50)) t)))
;; Google 广告过滤
(defun my-w3m-filter-rules-for-google (&rest args)
  "Filter rules for Google in w3m."
  (goto-char (point-min))
  (while (re-search-forward             ;remove publicize from google.cn or google.com
          "\\(赞助商链接\\|<h2>Sponsored Links</h2>\\).*aclk.*\\(</cite></ol><p>\\|在此展示您的广告\\)"
          nil t)
    (replace-match ""))
  (while (re-search-forward             ;remove publicize from google.com (English)
          "<h2>Sponsored Links</h2>.*aclk.*<h2>Search Results</h2>"
          nil t)
    (replace-match "")))
(eval-after-load "w3m-filter"
  '(add-to-list 'w3m-filter-rules
                '("\\`http://www\\.google\\.\\(cn\\|com\\)/"
                  my-w3m-filter-rules-for-google)))

;;; ### W3m ###
;;; --- 网页浏览器
(lazy-set-key
 '(("1" . emms-play-online)                             ;在线听音乐
   ("2" . kill-google-define-windows)                   ;关闭Google定义窗口
   ("3" . google-define)                                ;查找输入单词的Google定义
   ("4" . google-define-pointer)                        ;查找当前光标处的Google定义
   ("5" . w3m-open-rcirc-window)                        ;打开RCIRC窗口
   ("6" . w3m-session-save)                             ;保存浏览纪录
   ("7" . w3m-session-select)                           ;加载退出前的浏览器纪录
   ("8" . w3m-emacswiki-view-other-version)             ;查看当前wiki页面的其他版本
   ("9" . w3m-auto-install-elisp)                       ;自动安装elisp文件
   ("0" . w3m-gmail-toggle-mark)                        ;切换标记选项框
   ("(" . w3m-gmail-mark-all)                           ;标记选项框
   (")" . w3m-gmail-unmark-all)                         ;取消标记选项框
   ("c" . w3m-delete-buffer-and-select-right)           ;关闭当前标签并选择右边的标签
   ("/" . w3m-next-form)                                ;下一个表格处
   ("e" . w3m-scroll-down-or-previous-url)              ;向上翻页
   ("b" . w3m-edit-current-url)                         ;编辑当前页面
   ("z" . w3m-zoom-in-image)                            ;放大图片
   ("x" . w3m-zoom-out-image)                           ;缩小图片
   ("O" . w3m-goto-linknum)                             ;数字连接快速跳转
   ("f" . w3m-view-this-url)                            ;在当前标签打开
   ("o" . w3m-view-this-url-new-session)                ;在后台标签打开
   ("M" . w3m-open-link-in-chromium)                    ;Open link in chromium browser
   ("M-o" . w3m-open-link-file-under-current-directory) ;open link file under current directory
   ("m" . tabbar-forward-tab)                           ;切换到右边的标签
   ("n" . tabbar-backward-tab)                          ;切换到左边的标签
   ("'" . w3m-open-dead-link-with-external-browser)     ;打开死的连接
   ("s-j" . w3m-visual-scroll-up)                       ;可视化向上滚动
   ("s-k" . w3m-visual-scroll-down)                     ;可视化向下滚动
   ("b" . w3m-history)                                  ;历史
   ("D" . w3m-dtree)                                    ;显示本地目录树
   ("B" . w3m-view-previous-page)                       ;后退
   ("F" . w3m-view-next-page)                           ;前进
   ("S" . w3m-google-desktop-url-open)                  ;Google桌面打开连接
   ("L" . w3m-submit-form)                              ;提交form中的内容
   ("C" . w3m-delete-other-buffers)                     ;关闭后台标签
   ("d" . w3m-download-with-wget-current-position)      ;用Wget异步下载当前地连接
   ("Y" . wget-web-page)                                ;网页下载
   ("-" . org-w3m-copy-for-org-mode)                    ;转换网页成 `org-mode' 的链接格式
   ("_" . w3m-copy-link-in-region)                      ;拷贝w3m buffer 的所有链接
   ("&" . yaoddmuse-w3m-edit-emacswiki-page)            ;编辑 emacswiki 页面
   ("*" . w3m-emacswiki-view-diff)                      ;查看当前wiki页面的不同
   ("\"" . w3m-emacswiki-recent-changes)                ;最近的修改
   ("C-u s" . w3m-db-history)                           ;历史数据库
   ("<up>" . emms-volume-mode-plus)                     ;增加音量
   ("<down>" . emms-volume-mode-minus)                  ;减少音量
   ("<left>" . emms-seek-backward)                      ;后退
   ("<right>" . emms-seek-forward)                      ;前进
   ("<" . w3m-shift-left)                               ;向左滚动屏幕一像素
   (">" . w3m-shift-right)                              ;向右滚动屏幕一像素
   ("." . go-to-char-forward-word)                      ;向后查找某一个字符, 以单词为单位前进
   ("," . go-to-char-backward-word)                     ;向前查找某一个字符, 以单词为单位后退
   ("M-s" . lazy-search-menu)                           ;懒惰搜索
   ("M-A" . emms-pause)                                 ;暂停
   ("M-j" . scim-handle-event)                          ;设置为输入法切换键
   ("C-M-7" . w3m-tab-move-left)                        ;移动当前标签到左边
   ("C-M-8" . w3m-tab-move-right)                       ;移动当前标签到右边
   ("C-S-7" . w3m-delete-left-tabs)                     ;删除左边的标签
   ("C-S-8" . w3m-delete-right-tabs)                    ;删除右边的标签
   )
 w3m-mode-map
 )
;; (lazy-set-key sdcv-key-alist w3m-mode-map) ;sdcv的局部按键绑定
(lazy-unset-key
 '("s")
 w3m-mode-map)                          ;卸载按键
(lazy-set-key
 '(
   ("s" . one-key-menu-w3m-search)      ;w3m 搜索菜单
   )
 w3m-mode-map)

(provide 'init-w3m)

;;; init-w3m.el ends here
