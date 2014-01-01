;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;按键设置;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ### Sdcv ###
;;; --- 星际译王命令行
(defvar sdcv-key-alist nil
  "The key alist that sdcv.")
(setq sdcv-key-alist
      '(("p" . sdcv-search-pointer)     ;光标处的单词, buffer显示
        ("y" . sdcv-search-pointer+)    ;光标处的单词, tooltip显示
        ("i" . sdcv-search-input)       ;输入的单词, buffer显示
        (";" . sdcv-search-input+)))    ;输入的单词, tooltip显示
(lazy-set-key sdcv-key-alist nil "C-z") ;sdcv的全局按键绑定
;;; ### Vi-move ###
;;; --- Vi式移动
(defvar vi-move-key-alist nil
  "The key alist that like vi move.")
(setq vi-move-key-alist
      '(("j" . next-line)               ;上一行
        ("k" . previous-line)           ;下一行
        ("h" . backward-char)           ;向后移动
        ("l" . forward-char)            ;向前移动
        ("e" . scroll-down)             ;向下滚动一屏
        ("SPC" . scroll-up)))           ;向上滚动一屏
;;; ### Doi ###
;;; --- Do Or Insert
(defvar doi-key-alist nil
  "The key alist that like doi.")
(setq doi-key-alist
      '(("SPC" . doi-scroll-up)            ;向上滚动一屏
        ("e" . doi-scroll-down)            ;向下滚动一屏
        ("k" . doi-previous-line)          ;上一行
        ("j" . doi-next-line)              ;下一行
        ("h" . doi-backward-char)          ;后一个字符
        ("l" . doi-forward-char)           ;前一个字符
        ("J" . doi-scroll-up-one-line)     ;向上滚动一行
        ("K" . doi-scroll-down-one-line)   ;向下滚动一行
        ("H" . doi-backward-word)          ;后一个词
        ("L" . doi-forward-word)           ;前一个词
        ("y" . doi-sdcv-search-pointer+)   ;翻译
        ("s" . doi-isearch-forward)        ;向前搜索
        ("r" . doi-isearch-backward)       ;向后搜索
        ("A" . doi-move-beginning-of-line) ;移动到行首
        ("E" . doi-move-end-of-line)       ;移动到行末
        ("," . doi-end-of-buffer)          ;移动到buffer末尾
        ("." . doi-beginning-of-buffer)    ;移动到buffer开头
        ))
;;; ### Customize ###
;;; --- 自定义模式
(lazy-set-key doi-key-alist custom-mode-map)          ;doi 的局部按键绑定
;;; ### Compilation ###
;;; --- 编译模式
(lazy-set-key sdcv-key-alist compilation-mode-map)    ;sdcv 按键局部绑定
(lazy-set-key vi-move-key-alist compilation-mode-map) ;vi 模式按键局部绑定
;;; ### Winner-mode ###
;;; --- 窗口设置撤销或返回
(lazy-set-key
 '(
   ("s-<" . winner-undo)                ;窗口设置撤销
   ("s->" . winner-redo)                ;窗口设置返回
   ))
;;; ### Gtk2hs ###
(lazy-set-key
 '(("M-s-;" . gtk2hs-format-docs)
   ("M-s-'" . gtk2hs-format-fun)
   ("M-s-/" . gtk2hs-format-args)))
;;; ### Yoaddmuse ###
;;; --- Yet another oddmuse mode
(lazy-set-key
 '(
   ;; ("M-s-;" . one-key-menu-yaoddmuse)   ;yaoddmuse 菜单
   ))
;;; ### EMMS ###
;;; --- Emacs 多媒体系统
(lazy-set-key
 '(
   ("C-c p" . one-key-menu-emms)        ;播放器菜单
   ("<up>" . emms-volume-mode-plus)     ;增加音量
   ("<down>" . emms-volume-mode-minus)  ;减少音量
   ("<left>" . emms-seek-backward)      ;后退
   ("<right>" . emms-seek-forward)      ;前进
   ("M-A" . emms-pause)                 ;暂停/播放
   ("M-X" . emms-random)                ;随机播放
   ("M-Z" . emms-stop)                  ;停止
   ))
;;; ### Emms Playlist ###
;;; --- EMMS 播放列表
(lazy-unset-key
 '("s" "m" "u" "M-<" "M->")
 emms-playlist-mode-map)                ;卸载按键
(lazy-set-key
 '(
   ("C-x C-s" . emms-playlist-save)             ;保存播放列表
   ("C-y" . emms-playlist-mode-yank)            ;剪切
   ("C-k" . emms-playlist-mode-kill-track)      ;删除当前TRACK
   ("C-w" . emms-playlist-mode-kill)            ;删除
   ("C-/" . emms-playlist-mode-undo)            ;撤销
   ("J" . scroll-up-one-line)                   ;向上滚动一行
   ("K" . scroll-down-one-line)                 ;向下滚动一行
   ("." . emms-playlist-mode-first)             ;浏览最上面一行
   ("," . emms-playlist-mode-last)              ;浏览最下面一行
   ("C-j" . emms-playlist-mode-insert-newline)  ;新建一行
   ("M-y" . emms-playlist-mode-yank-pop)        ;YANK弹出
   ("M-n" . emms-playlist-mode-next)            ;下一个播放列表
   ("M-p" . emms-playlist-mode-previous)        ;上一个播放列表
   ("a" . emms-playlist-mode-add-contents)      ;向当前播放列表添加内容
   ("d" . emms-playlist-mode-kill-entire-track) ;从播放列表中移除当前TRACK
   ("C" . emms-playlist-mode-clear)             ;清空当前的播放列表
   ("f" . emms-playlist-mode-play-smart)        ;播放当前TRACK
   ("b" . emms-playlist-set-playlist-buffer)    ;设定当前播放列表BUFFER
   ("n" . emms-next)                            ;播放下一首
   ("p" . emms-previous)                        ;播放上一首
   ("r" . emms-random)                          ;随机播放下一首
   (">" . emms-seek-forward)                    ;前进
   ("<" . emms-seek-backward)                   ;后退
   ("X" . emms-pause)                           ;暂停
   ("T" . emms-stop)                            ;停止
   ("Z" . emms-show)                            ;显示播放信息
   ("q" . emms-playlist-mode-bury-buffer)       ;退出
   ("?" . describe-mode)                        ;帮助
   ("g" . emms-playlist-mode-center-current)    ;跳转到当前播放TRACK
   ("G" . emms-jump-to-file)                    ;定位当前音乐文件的位置
   ("D" . emms-delete-file-from-disk)           ;丛磁盘删除当前的文件
   (";" . emms-tag-editor-edit-marked-tracks)   ;编辑标记的TAG
   ("H" . emms-last-mark-track)                 ;最后一个标记
   ("L" . emms-first-mark-track)                ;第一个标记
   ("N" . emms-next-mark-track)                 ;下一个标记
   ("P" . emms-prev-mark-track)                 ;上一个标记
   ("s" . one-key-menu-emms-playlist-sort)      ;列表排序菜单
   ("m" . one-key-menu-emms-playlist-mark)      ;列表标记菜单
   )
 emms-playlist-mode-map
 )
(lazy-set-key vi-move-key-alist emms-playlist-mode-map) ;vi-move 的局部按键
;;; ### Python ###
;;; --- Python mode
(lazy-set-key
 '(
   ("C-S-j" . jump-to-import)
   )
 python-mode-map)
;;; ### JS2 Mode ###
;;; --- JS2 模式
(lazy-set-key
 '(
   ("C-x '" . js2-next-error)
   )
 js2-mode-map)
;;; ### Emms Tag Editor ###
;;; --- Emms 标签编辑器
(lazy-set-key
 '(
   ("C-c C-j" . emms-tag-editor-next-same-field)  ;下一个相同的区域
   ("C-c C-k" . emms-tag-editor-prev-same-field)  ;上一个相同的区域
   ("C-c C-r" . emms-tag-editor-set-all+)         ;替换所有标签
   ("C-c C-l" . emms-tag-editor-set-tracknumber)  ;插入轨迹序号, 要确认
   ("C-c C-i" . emms-tag-editor-set-tracknumber+) ;插入轨迹序号, 不用确认
   )
 emms-tag-editor-mode-map
 )
;;; ### EMMS Browser ###
;;; --- EMMS 浏览器
(lazy-set-key
 '(
   ("J" . emms-browser-next-non-track)      ;下一个节点
   ("K" . emms-browser-prev-non-track)      ;上一个节点
   ("f" . emms-browser-toggle-subitems)     ;显示
   ("s" . one-key-menu-emms-browser-search) ;搜索菜单
   ("L" . one-key-menu-emms-browser-lookup) ;查询菜单
   )
 emms-browser-mode-map
 )
(lazy-set-key sdcv-key-alist emms-browser-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist emms-browser-mode-map) ;vi-move 的局部按键
;;; ### EMMS Stream ###
;;; --- EMMS 流媒体
(lazy-set-key
 '(
   ("a" . emms-stream-add-bookmark)          ;添加
   ("d" . emms-stream-delete-bookmark)       ;删除
   ("E" . emms-stream-edit-bookmark)         ;编辑
   ("q" . emms-stream-quit)                  ;退出
   ("s" . emms-stream-save-bookmarks-file)   ;保存
   ("t" . emms-stream-toggle-default-action) ;切换
   ("i" . emms-stream-info-bookmark)         ;信息
   ("f" . emms-stream-play)                  ;播放
   )
 emms-stream-mode-map
 )
(lazy-set-key vi-move-key-alist emms-stream-mode-map) ;vi-move 的局部按键
;;; ### Etags ###
;;; --- 代码搜索
(lazy-set-key
 '(
   ("s-E" . one-key-menu-etags)))
;;; ### IRC ###
;;; --- 聊天
(lazy-set-key
 '(
   ("C-c i" . switch-to-erc)                     ;切换到IRC或自动登录IRC
   ("C-c I" . erc-nick-notify-jump-last-channel) ;自动跳转到最后收到消息的频道
   ("M-U" . one-key-menu-irc-channel)            ;跳转到IRC频道
   ))
;;; ### ERC ###
;;; --- IRC 客户端
(lazy-set-key
 '(
   ("C-c C-y" . paste2-buffer-create)   ;粘贴大段内容
   ("/" . doi-erc-command)              ;erc命令
   )
 erc-mode-map
 )
(lazy-set-key doi-key-alist erc-mode-map) ;doi 的局部按键
;;; ### Rcirc ###
;;; --- IRC 客户端
(lazy-set-key
 '(
   ("M-o" . backward-delete-char-untabify)
   ("M-O" . rcirc-omit-mode)                    ;切换忽略模式
   ("C-c SPC" . rcirc-cmd-read-current-message) ;阅读当前消息
   ("C-c C-v" . rcirc-cmd-talk-to)              ;键入某人的昵称
   )
 rcirc-mode-map
 )
(lazy-set-key doi-key-alist rcirc-mode-map) ;doi 的局部按键
;;; ### Auto-Install ###
;;; --- 自动下载安装elisp文件
(lazy-set-key
 '(
   ("C-z e" . anything-auto-install)    ;从多种方式安装 elisp 包
   ))
;;; ### Paste2 ###
;;; --- paste2.org 的粘贴服务
(lazy-set-key
 '(
   ("C-s-s" . paste2-send-paste)                     ;发送粘贴
   ("C-s-z" . paste2-get-paste)                      ;得到粘贴的内容
   ("C-s-a" . paste2-buffer-append)                  ;附加当前的内容到粘贴buffer
   ("C-s-x" . paste2-buffer-create)                  ;创建粘贴buffer
   ("C-s-c" . paste2-send-paste-with-command-output) ;粘贴命令行输出
   ))
;;; ### Alarm Clock ###
;;; --- 闹钟
(lazy-set-key
 '(
   ("s-x s-s" . alarm-clock)            ;设定消息提醒
   ("s-x s-c" . alarm-clock-cancel)     ;取消消息提醒
   ))
;;; ### irfc ###
;;; --- RFC 文档阅读
(lazy-set-key sdcv-key-alist irfc-mode-map)
(lazy-set-key
 '(
   ("c" . kill-this-buffer)                         ;关闭当前buffer
   ("C" . kill-current-mode-buffers-except-current) ;关闭所有后台标签
   ("m" . tabbar-forward-tab)                       ;向右一个标签
   ("n" . tabbar-backward-tab)                      ;向左一个标签
   ("<" . end-of-buffer)                            ;最下面
   (">" . beginning-of-buffer)                      ;最上面
   )
 irfc-mode-map
 )
;;; ### Less ###
;;; --- 快速浏览模式
(lazy-set-key
 '(
   ("M-s-l" . less-minor-mode)          ;打开less模式
   ))
(lazy-set-key
 '(
   ("J" . less-scroll-up-one-line)      ;向下浏览
   ("K" . less-scroll-down-one-line)    ;向上浏览
   ("." . go-to-char-forward)           ;向后查找某一个字符
   ("," . go-to-char-backward)          ;向前查找某一个字符
   (">" . beginning-of-buffer)          ;BUFFER结尾
   ("<" . end-of-buffer)                ;BUFFER开始
   ("q" . less-quit)                    ;退出less模式
   ("b" . one-key-menu-hideshow)        ;hideshow 菜单
   ("t" . one-key-menu-etags)           ;Etags 菜单
   ("dd" . auto-scroll-mode)            ;开始滚屏
   ("df" . auto-scroll-faster)          ;滚动的快一点
   ("ds" . auto-scroll-slower)          ;滚动的慢一点
   )
 less-minor-mode-map
 )
(lazy-set-key sdcv-key-alist less-minor-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist less-minor-mode-map) ;vi-move 的局部按键
;;; ### Hexl ###
;;; --- 十六进制模式
(lazy-set-key
 '(
   ("s-c hh" . hexl-mode)               ;十六进制模式
   ("s-c hf" . hexl-find-file)          ;以十六进制打开文件
   ))
;;; ### Compile DWIM ###
;;; --- 智能编译
(lazy-set-key
 '(
   ("C-9" . compile-dwim-compile+)      ;智能编译程序
   ("C-0" . compile-dwim-run)           ;智能运行程序
   ))
;;; ### Re-builder ###
;;; --- 可视化正则表达式建立
(lazy-set-key
 '(
   ("C-c b" . reb-change-target-buffer) ;改变目标buffer
   ("C-c c" . reb-toggle-case)          ;切换大小写
   ("C-c e" . reb-enter-subexp-mode)    ;进入表达式模式
   ("C-c r" . reb-prev-match)           ;前一个匹配
   ("C-c s" . reb-next-match)           ;后一个匹配
   ("C-c u" . reb-force-update)         ;更新
   ("C-c w" . reb-copy)                 ;拷贝
   ("C-c q" . reb-quit)                 ;退出
   ("C-c TAB" . reb-change-syntax)      ;改变语法
   )
 reb-mode-map
 )
;;; ### Company Mode ###
;;; --- 直观的列表式补全
;; (lazy-unset-key
;;  '("TAB")
;;  company-mode-map)                      ;卸载按键
;; (lazy-unset-key
;;  '("M-p" "M-n" "M-1"
;;    "M-2" "M-3" "M-4"
;;    "M-5" "M-6" "M-7"
;;    "M-8" "M-9" "M-0"
;;    "C-m")
;;  company-active-map)
;; (lazy-set-key
;;  '(
;;    ("M-h" . company-complete-common)    ;补全公共部分
;;    ("M-H" . company-complete-selection) ;补全选择的
;;    ("M-w" . company-show-location)      ;显示局部的
;;    ("M-s" . company-search-candidates)  ;搜索候选
;;    ("M-S" . company-filter-candidates)  ;过滤候选
;;    ("M-," . company-select-next)        ;下一个
;;    ("M-." . company-select-previous)    ;上一个
;;    )
;;  company-active-map
;;  )
;;; ### Auto-complete ###
;;; --- 自动补全
(lazy-unset-key
 '("RET" "TAB")
 ac-complete-mode-map)
(lazy-set-key
 '(
   ("M-h" . ac-complete)                ;补全当前选中的
   ("M-H" . ac-expand-common)           ;补全公共部分
   ("M-U" . ac-stop)                    ;停止
   ("M-," . ac-next)                    ;下一个
   ("M-." . ac-previous)                ;上一个
   )
 ac-complete-mode-map
 )
;;; ### C ###
;;; --- C 语言模式
(lazy-set-key
 '(
   ("M-j" . toggle-input-method)                  ;切换输入法
   ("M-[" . my-c-function-init-indent)            ;函数括号格式化
   ("M-'" . my-c-mode-auto-newline-break)         ;C模式auto-newline最后一空行跳出, 到花括号右边
   ("M-," . my-c-mode-auto-newline-break-newline) ;C模式auto-newline最后一空行跳出, 并换行
   ("RET" . newline-and-indent)                   ;当在C语言模式时按RET自动新行和对齐
   ("M-:" . my-c-previous-line-comment)           ;C语言上一行注释
   ("M-s-m" . eassist-list-methods)               ;选择方法
   )
 c-mode-base-map
 )
;;; ### Xgtags ###
;;; --- Gtags 的界面, 快速代码搜索
(lazy-set-key
 '(
   ("s-G" . one-key-menu-gtags)         ;gtags 按键
   ))
(lazy-set-key vi-move-key-alist xgtags-select-mode-map) ;vi-move 的局部按键
(lazy-set-key sdcv-key-alist xgtags-select-mode-map)    ;sdcv的局部按键
(lazy-set-key
 '(
   ("f" . xgtags-select-tag-other-window)   ;在其他窗口选择TAG视图
   ("r" . xgtags-select-tag-return)         ;返回选择前的窗口配置
   ("J" . xgtags-select-next-tag-line)      ;下一个引用行
   ("K" . xgtags-select-prev-tag-line)      ;上一个引用行
   ("N" . xgtags-select-next-tag-line-show) ;在其他窗口显示下一个引用
   ("P" . xgtags-select-prev-tag-line-show) ;在其他窗口显示上一个引用
   ("H" . xgtags-select-next-file)          ;下一个文件
   ("L" . xgtags-select-prev-file)          ;上一个文件
   )
 xgtags-select-mode-map
 )
;;; ### Emacs-Lisp ###
;;; --- Emacs Lisp 模式
(lazy-unset-key
 '("M-TAB")
 emacs-lisp-mode-map)                   ;卸载按键
(lazy-set-key
 '(
   ("<s-f1>" . elisp-index-search+)     ;在elisp手册中查找函数
   ))
(lazy-set-key
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map
 )
;;; ### Wget ###
;;; --- 下载程序
(lazy-set-key
 '(
   ("s-c dd" . wget-show)               ;显示下载信息
   ("s-c dh" . wget-hide)               ;隐藏下载信息
   ("s-c dq" . wget-quit-and-exit)      ;停止下载
   ))
;;; ### Hideshow ###
;;; --- 代码折叠
(lazy-set-key
 '(
   ("C-c b" . one-key-menu-hideshow)    ;结构化菜单
   ))
;;; ### Google Define ###
;;; --- Google 定义
(lazy-set-key
 '(
   ("C-z g" . google-define)              ;查找输入单词的Google定义
   ("C-z s" . google-define-pointer)      ;查找当前光标单词的Google定义
   ("C-z h" . kill-google-define-windows) ;关闭Google定义窗口
   ))
;;; ### Org ###
;;; --- 笔记管理和组织

(lazy-set-key
 '(
   ("s-u" . one-key-menu-org-mode)           ;Org-mode 菜单
   ("s-U" . one-key-menu-org-mode-recursive) ;Org-mode 菜单, 但是递归的
   ("M-O" . org-display-all-todo-item)       ;显示所有TODO列表
   )
 org-mode-map
 )
(lazy-set-key
 '(
   ("s-s" . one-key-menu-org-file)      ;Org 文件
   ("C-c r" . org-remember)             ;Org-remeber
   ))
;;; ### Newsticker ###
;;; --- 新闻阅读器
(lazy-set-key
 '(
   ("M-D" . newsticker-show-news)       ;打开新闻阅读器
   ))
(defconst newsticker-treeview-mode-map
  (let ((map (make-sparse-keymap 'newsticker-treeview-mode-map)))
    (lazy-set-key
     '(
       ("i" . newsticker-treeview-toggle-item-immortal)              ;切换保持状态
       ("e" . newsticker-treeview-prev-page)                         ;上一屏
       (" " . newsticker-treeview-next-page)                         ;下一屏
       ("." . newsticker-treeview-scroll-item+)                      ;向上滚动浏览窗口
       ("," . newsticker-treeview-scroll-item)                       ;向下滚动浏览窗口
       ("L" . newsticker-treeview-first-feed)                        ;第一个种子
       ("H" . newsticker-treeview-last-feed)                         ;最后一个种子
       ("P" . newsticker-treeview-prev-feed)                         ;上一个种子
       ("N" . newsticker-treeview-next-feed)                         ;下一个种子
       ("j" . newsticker-treeview-next-item)                         ;下一个项目
       ("k" . newsticker-treeview-prev-item)                         ;上一个项目
       ("K" . newsticker-treeview-prev-new-or-immortal-item)         ;上一个新的条目
       ("J" . newsticker-treeview-next-new-or-immortal-item)         ;下一个新的条目
       ("g" . newsticker-treeview-get-news)                          ;抓取当前种子的新闻
       ("v" . newsticker-treeview-jump)                              ;跳转
       ("O" . newsticker-treeview-mark-list-items-old)               ;标记整个列表的条目为旧的
       ("o" . newsticker-treeview-mark-item-old)                     ;标记条目为旧的
       ("q" . newsticker-treeview-quit)                              ;隐藏
       ("S" . newsticker-treeview-save-item)                         ;保存网页
       ("s" . newsticker-treeview-save)                              ;保存条目
       ("u" . newsticker-treeview-update)                            ;更新
       ("f" . newsticker-treeview-browse-url-with-w3m)               ;浏览连接
       ("a" . newsticker-add-url)                                    ;添加连接
       ("G" . newsticker-get-all-news)                               ;抓取所有新闻
       ("w" . newsticker-switch-to-w3m)                              ;切换到w3m视图
       ("m" . newsticker--treeview-browse-url-with-firefox)          ;用外部浏览器浏览
       ("M" . newsticker--treeview-browse-url-with-chromium-browser) ;用外部浏览器浏览
       ("M-m" . newsticker-group-move-feed)                          ;移动种子到组
       ("M-a" . newsticker-group-add-group)                          ;添加组
       )
     map
     )
    map)
  "Mode map for newsticker treeview.")
;;; ### Revie ###
;;; --- 窗口配置管理
(lazy-set-key
 '(
   ("s-v s" . save-current-configuration) ;保存当前的窗口配置方案
   ("s-v f" . resume)                     ;恢复上一次的窗口配置方案
   ("s-v k" . wipe)                       ;清空窗口配置方案
   ))
;;; ### Multi-Scratch ###
;;; --- 多重草稿
(lazy-set-key
 '(
   ("C-1" . multi-scratch-prev)         ;上一个草稿
   ("C-2" . multi-scratch-next)         ;下一个草稿
   ("C-3" . multi-scratch-new)          ;新建草稿
   ))
;;; ### Ido ###
;;; --- 交互式管理文件和缓存
(lazy-set-key
 '(
   ("C-x C-f" . ido-find-file)          ;交互式查找文件
   ("C-x b" . ido-switch-buffer)        ;交互式切换buffer
   ("C-x i" . ido-insert-buffer)        ;插入缓存
   ("C-x I" . ido-insert-file)          ;插入文件
   ))
(add-hook 'ido-setup-hook
          '(lambda ()
             (interactive)
             (ido-my-keys ido-completion-map)))
(defun ido-my-keys (keymap)
  "Add my keybindings for ido."
  (lazy-set-key
   '(
     ("M-s-p" . ido-prev-match)              ;上一个匹配
     ("M-s-n" . ido-next-match)              ;下一个匹配
     ("M-s-h" . ido-next-work-directory)     ;下一个工作目录
     ("M-s-l" . ido-prev-work-directory)     ;上一个工作目录
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("M-O" . ido-delete-backward-updir)     ;删除字符或进入上一级目录
     )
   keymap
   ))
;; ### Icicles ###
;; --- Minibuffer 输入补全和切换
(add-hook 'icicle-mode-hook 'bind-icicles-minibuffer-keys)
(defun bind-icicles-minibuffer-keys ()
  "Replace some default Icicles minibuffer bindings with others."
  (dolist
      (map (list
            minibuffer-local-isearch-map             ;isearch
            minibuffer-local-ns-map                  ;当空格不允许时
            minibuffer-local-shell-command-map       ;补全shell命令时
            minibuffer-local-map                     ;从minibuffer读取
            minibuffer-local-completion-map          ;输入补全
            minibuffer-local-must-match-map          ;输入补全精确匹配
            minibuffer-local-filename-completion-map ;文件名补全
            ))
    (when icicle-mode
      (lazy-set-key
       '(
         ("s-o" . icicle-insert-history-element) ;插入历史元素
         )
       map
       )
      (ido-my-keys map)))
  (when icicle-mode
    (lazy-set-key
     '(
       ("TAB" . isearch-complete-edit)
       ("M-k" . isearch-delete-ring-element))
     minibuffer-local-isearch-map
     )))
(lazy-set-key
 '(
   ("M-s-z" . icicle-switch-to-Completions-buf) ;切换到提示buffer
   ("M-s-x" . icicle-switch-to/from-minibuffer) ;在minibuffer和其他buffer之间切换
   ("M-s-m" . icicle-complete-keys)             ;查看当前模式的按键
   ))
;;; ### Maxima ###
;;; --- 代数计算系统
(lazy-set-key
 '(
   ("s-A" . my-imaxima)                 ;代数计算系统
   ))
(defun my-maxima-keybind ()             ;代数计算系统按键
  (lazy-set-key
   '(
     ("TAB" . maxima-complete)          ;补全
     ("C-p" . comint-previous-input)    ;上一个输入
     ("C-n" . comint-next-input)        ;下一个输入
     ("M-p" . go-to-next-pair-right)    ;括号跳转
     ("M-n" . go-to-next-pair-left))
   (current-local-map)
   ))
;;; ### Pick-backup ###
;;; --- 快速恢复对比备份文件
(lazy-set-key
 '(
   ("s-b v" . pick-backup-and-view)     ;查看备份版本
   ("s-b d" . pick-backup-and-ediff)    ;比较备份版本的不同
   ("s-b r" . pick-backup-and-revert)   ;恢复指定的备份版本
   ))
;;; ### Flymake ###
;;; --- 及时拼写检查
(lazy-set-key
 '(
   ("M-s-j" . flymake-show-next-error)  ;显示下一个错误
   ("M-s-k" . flymake-show-prev-error)  ;显示上一个错误
   ))
;;; ### Speedbar ###
;;; --- 快速访问文件和tags
(setq speedbar-buffers-key-map nil)     ;卸载一些按键
(setq speedbar-file-key-map nil)
(lazy-set-key
 '(
   ("s-z s-z" . sr-speedbar-toggle)        ;显示/隐藏speedbar
   ("s-z s-x" . sr-speedbar-select-window) ;选中speedbar窗口
   ))
(lazy-set-key
 '(
   ;; 导航操作
   ("f" . speedbar-edit-line)             ;进入当前条目
   ("C-m" . speedbar-edit-line)           ;进入当前条目
   ("j" . speedbar-next)                  ;下一行
   ("k" . speedbar-prev)                  ;上一行
   ("n" . speedbar-forward-list)          ;下一条目
   ("p" . speedbar-backward-list)         ;上一条目
   ("u" . speedbar-forced-contract)       ;跳到上一级
   ("F" . speedbar-files)                 ;切换文件视图
   ("B" . speedbar-buffers)               ;切换缓存视图
   ("q" . sr-speedbar-toggle)             ;退出
   ;; 树操作
   ("x" . speedbar-expand-line)           ;展开当前行
   ("z" . speedbar-contract-line)         ;收缩当前行
   ("v" . speedbar-toggle-line-expansion) ;切换当前行的状态
   ;; 文件操作
   ("g" . speedbar-refresh)             ;刷新
   ("'" . speedbar-up-directory)        ;上一级目录
   ("i" . speedbar-item-info)           ;显示信息
   ("b" . speedbar-item-byte-compile)   ;编译
   ("l" . speedbar-item-load)           ;加载
   ("c" . speedbar-item-copy)           ;拷贝
   ("d" . speedbar-item-delete)         ;删除
   ("o" . speedbar-item-object-delete)  ;删除对象
   ("r" . speedbar-item-rename)         ;重命令
   ("m" . speedbar-create-directory)    ;创建目录
   ("K" . speedbar-buffer-kill-buffer)  ;关闭当前buffer
   )
 speedbar-key-map
 )
;;; ### Top ###
;;; --- 进程管理器
(lazy-set-key
 '(
   ("<s-f8>" . top)                     ;TOP
   ))
(lazy-set-key
 '(
   ("s" . isearch-forward)              ;搜索
   ("g" . top)                          ;刷新
   ("q" . quit-window)                  ;退出
   ("d" . top-mode-kill)                ;删除
   ("D" . top-mode-kill-noconfirm)      ;不需要确认删除
   ("t" . top-mode-strace)
   ("T" . top-mode-strace-noconfirm)
   ("r" . top-mode-renice)
   ("R" . top-mode-renice-noconfirm)
   ("m" . top-mode-mark)                ;标记
   ("u" . top-mode-unmark)              ;删除标记
   ("U" . top-mode-show-specific-user))
 top-mode-map
 )
(lazy-set-key sdcv-key-alist top-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist top-mode-map) ;vi-mode的局部按键
;;; ### Doc-view ###
;;; --- 文档阅读器
(lazy-unset-key
 '(".")
 doc-view-mode-map)                     ;卸载按键
(lazy-set-key
 '(
   ("C-M-j" . doc-view-scroll-up-or-next-page+)       ;翻另一个窗口中图书的下一页
   ("C-M-k" . doc-view-scroll-down-or-previous-page+) ;翻另一个窗口中图书的上一页
   ))
(lazy-unset-key
 '("x" "M-<" "M->")
 doc-view-mode-map)                     ;卸载一些按键
(lazy-set-key
 '(
   ([remap scroll-up] . doc-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
   )
 doc-view-mode-map
 )
(lazy-set-key
 '(
   ("N" . doc-view-next-page)                      ;下一页
   ("P" . doc-view-previous-page)                  ;上一页
   ("." . doc-view-first-page)                     ;第一页
   ("," . doc-view-last-page)                      ;最后一页
   ("g" . doc-view-goto-page)                      ;跳到第几页
   ("e" . doc-view-scroll-down-or-previous-page)   ;向上滚动一屏
   ("SPC" . doc-view-scroll-up-or-next-page)       ;向下滚动一屏
   ("j" . doc-view-next-line-or-next-page)         ;下一行或下一屏
   ("k" . doc-view-previous-line-or-previous-page) ;上一行或上一屏
   ("t" . doc-view-show-tooltip)                   ;当前页提示
   ("q" . bury-buffer)                             ;隐藏buffer
   ("Q" . doc-view-kill-proc-and-buffer)           ;退出并结束进程
   ("C-s" . doc-view-search)                       ;搜索
   ("C-S-n" . doc-view-search-next-match)          ;下一个匹配
   ("C-S-p" . doc-view-search-previous-match)      ;上一个匹配
   ("+" . doc-view-enlarge)                        ;放大页面
   ("-" . doc-view-shrink)                         ;缩小页面
   ("C-c C-c" . doc-view-toggle-display)           ;在文本和图像间切换
   ("C-c C-t" . doc-view-open-text)                ;打开文本
   ("r" . revert-buffer)                           ;刷新
   ("s" . auto-scroll-mode)                        ;自动滚屏
   ("<" . auto-scroll-faster)                      ;加快滚屏速度
   (">" . auto-scroll-slower)                      ;减慢滚屏速度
   )
 doc-view-mode-map
 )
(lazy-set-key sdcv-key-alist doc-view-mode-map) ;sdcv的局部按键绑定
;;; ### Gnus ###
;;; --- 新闻阅读器
(lazy-unset-key
 '("M-K" "s")
 gnus-summary-mode-map)                 ;卸载按键
(lazy-set-key
 '(
   ("M-E" . gnus-switch)                ;切换 Gnus
   ))
(add-hook 'gnus-group-mode-hook
          (lambda ()
            (local-set-key (kbd "q") 'gnus-switch)      ;切换Gnus
            (local-set-key (kbd "Q") 'gnus-group-exit)) ;退出
          )
(lazy-set-key
 '(
   ("s" . one-key-menu-gnus-summary-sort)     ;邮件排序
   ("S" . gnus-summary-isearch-article)       ;搜索邮件
   ("f" . gnus-summary-next-page)             ;显示邮件
   ("v" . gnus-summary-followup)              ;跟随, 但不引用原作者的邮件
   ("E" . gnus-summary-edit-article)          ;编辑邮件
   ("d" . gnus-summary-delete-article)        ;删除邮件
   ("y" . gnus-summary-select-article-buffer) ;显示对应的 article
   )
 gnus-summary-mode-map
 )
(lazy-set-key
 '(
   ("y" . gnus-article-show-summary)    ;在摘要和相应文章之间跳转
   ("x" . gnus-mime-save-part)          ;保存mime部分
   )
 gnus-article-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-summary-mode-map) ;vi-move 的局部按键
(lazy-unset-key '("t" "T") gnus-group-mode-map)        ;卸载一些按键
(lazy-set-key
 '(
   ("f" . gnus-group-read-group-no-prompt) ;读取组, 不提醒
   ("K" . gnus-group-list-groups)          ;列出组
   ("t" . one-key-menu-gnus-topic-edit)    ;编辑菜单
   ("T" . one-key-menu-gnus-topic-sort)    ;排序菜单
   )
 gnus-group-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-group-mode-map) ;vi-move 的局部按键
(lazy-set-key
 '(
   ("f" . gnus-server-read-server)      ;读取服务器
   ("d" . gnus-server-kill-server)      ;删除服务器
   )
 gnus-server-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-server-mode-map) ;vi-move 的局部按键
(lazy-set-key
 '(
   ("J" . scroll-up-one-line)           ;向上滚动一行
   ("K" . scroll-down-one-line)         ;向下滚动一行
   ("f" . gnus-browse-read-group)       ;阅读当前组
   )
 gnus-browse-mode-map
 )
(lazy-set-key vi-move-key-alist gnus-browse-mode-map) ;vi-move 的局部按键
;;; ### Haskell ###
;;; --- Haskell 语言模式
(lazy-set-key
 '(
   ("M-;" . comment-dwim-with-haskell-style) ;注释
   )
 haskell-mode-map
 )
;;; ### Contentswitch ###
;;; --- 按内容快速搜索
(lazy-set-key
 '(
   ("C-S-s-y" . contentswitch)          ;按内容进行搜索
   ))
(lazy-set-key
 '(
   ("C-n" . contentswitch-next-line)     ;下一行
   ("C-p" . contentswitch-previous-line) ;上一行
   ("C-v" . contentswitch-next-page)     ;下一屏
   ("M-v" . contentswitch-previous-page) ;上一屏
   ("C-m" . exit-minibuffer)             ;选择搜索结果
   )
 contentswitch-map
 )
;;; ### Anything ###
;;; --- 快速buffer切换
(lazy-set-key
 '(("s-y" . anything)                   ;anything
   ("C-s-y" . anything-call-source)     ;调用特定的源
   ))
(lazy-set-key
 '(
   ("C-n" . anything-next-line)                  ;下一行
   ("C-p" . anything-previous-line)              ;上一行
   ("C-s" . anything-isearch)                    ;搜索
   ("C-m" . anything-exit-minibuffer)            ;执行动作, 并退出
   ("C-j" . anything-execute-persistent-action)  ;执行动作, 但不退出
   ("C-v" . anything-next-page)                  ;下一页
   ("M-v" . anything-previous-page)              ;上一页
   ("M-s-y" . anything-insert-selection)         ;插入当前项目
   ("M-s-i" . anything-insert-symbol)            ;插入当前符号
   ("M-s-o" . anything-insert-buffer-name)       ;插入缓存名字
   ("M-s-j" . anything-next-source)              ;下一个种类
   ("M-s-k" . anything-previous-source)          ;上一个种类
   ("M-s-h" . anything-select-action)            ;选择动作或切换回源
   ("M-s-l" . anything-select-source)            ;选择源
   ("M-s-n" . anything-next-history-element)     ;下一个历史记录
   ("M-s-p" . anything-previous-history-element) ;上一个历史记录
   )
 anything-map
 )
;;; ### Timid ###
;;; --- 快速补全
(setq timid-keys
      (quote (("C-m" . timid-select-file)   ;选择
              ("ESC" . timid-cleanup)       ;清理
              ("C-p" . timid-previous-line) ;上一行
              ("C-n" . timid-next-line)     ;下一行
              ("M-v" . timid-previous-page) ;上一页
              ("C-v" . timid-next-page))))  ;下一页
;;; ### Apropos ###
;;; --- 程序员命令查询
(lazy-set-key
 '(
   ("C-m" . apropos-follow)                ;进入
   ("N" . forward-button-with-line-begin)  ;下一个条目
   ("P" . backward-button-with-line-begin) ;上一个条目
   ("J" . scroll-up-one-line)              ;向上滚动一行
   ("K" . scroll-down-one-line)            ;向下滚动一行
   ("q" . quit-window)                     ;退出
   ("f" . push-button)                     ;确定
   )
 apropos-mode-map
 )
(lazy-set-key sdcv-key-alist apropos-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist apropos-mode-map) ;vi-move 的局部按键
;;; ### Sys-apropos ###
;;; --- 系统相关查询
(lazy-set-key
 '(
   ("f" . sys-apropos-run-woman)        ;查找
   )
 sys-apropos-mode-map
 )
(lazy-set-key sdcv-key-alist sys-apropos-mode-map)    ;sdcv 的局部按键
(lazy-set-key vi-move-key-alist sys-apropos-mode-map) ;vi-mode 的局部按键
;;; ### Help ###
;;; --- 帮助模式
;;(lazy-unset-key
;; '("e" "h" "y")
;; view-mode-map)                         ;卸载按键
(lazy-set-key
 '(
   ("C-h". one-key-menu-help)           ;帮助菜单
   ))
(lazy-set-key
 '(
   ("s-j" . one-key-menu-android)
   ))
;;; ### Info ###
;;; --- Info 模式
(lazy-set-key
 '(
   ("f" . Info-follow-nearest-node)     ;进入当前节点
   ("<tab>" . Info-next-reference)      ;下一个引用
   ("<backtab>" . Info-prev-reference)  ;上一个引用
   ("E" . Info-edit)                    ;编辑
   ("?" . Info-summary)                 ;帮助
   ("N" . Info-next)                    ;下一个同级节点
   ("P" . Info-prev)                    ;上一个同级节点
   ("J" . scroll-up-one-line)           ;向下滚动一行
   ("K" . scroll-down-one-line)         ;向上滚动一行
   ("." . go-to-char-forward)           ;向后查找某一个字符
   ("," . go-to-char-backward)          ;向前查找某一个字符
   ("<" . Info-forward-node)            ;下一个节点
   (">" . Info-backward-node)           ;上一个节点
   ("C-<" . Info-final-node)            ;最后一个节点
   ("C->" . Info-top-node)              ;最前一个节点
   ("s" . Info-search)                  ;搜索
   ("S" . Info-search-case-sensitively) ;区分大小写搜索
   ("g" . Info-goto-node)               ;跳到指定的节点
   ("q" . Info-exit)                    ;退出
   ("m" . Info-menu)                    ;菜单补全
   ("d" . Info-directory)               ;总目录
   ("I" . Info-index)                   ;索引
   ("o" . Info-follow-reference)        ;随后的引用补全
   ("H" . Info-history)                 ;历史
   ("F" . Info-history-forward)         ;历史向前
   ("B" . Info-history-back)            ;历史向后
   ("M-s" . Info-search)                ;节点搜索
   ("C" . clone-buffer)                 ;克隆当前buffer
   ("c" . Info-copy-current-node-name)  ;拷贝当前节点名字
   ("u" . Info-up)                      ;跳到上一级
   ("T" . Info-toc)                     ;内容索引
   ("e" . Info-scroll-down)             ;向上滚动, vi-move 的后面重新加载
   (" " . Info-fscroll-up)              ;向下滚动
   )
 Info-mode-map
 )
(lazy-set-key sdcv-key-alist Info-mode-map)    ;sdcv的局部按键绑定
(lazy-set-key vi-move-key-alist Info-mode-map) ;vi-move 的局部按键
;;; ### Undo Browse ###
;;; --- 强大的撤销系统
;; (lazy-set-key
;;  '(
;;    ("C-s-?" . ub-mode-on)               ;打开撤销系统
;;    ))
;; (lazy-set-key
;;  '(
;;    ("m" . ub-movie)                     ;电影观看
;;    ("s" . ub-movie-stop)                ;电影停止
;;    ("n" . ub-movie-forward)             ;下一个电影
;;    ("p" . ub-movie-backward)            ;上一个电影
;;    ("S" . ub-movie-history)             ;电影历史
;;    ("f" . ub-frame-forward)             ;前一帧
;;    ("b" . ub-frame-backward)            ;后一帧
;;    ("A" . ub-frame-beginning)           ;第一帧
;;    ("E" . ub-frame-end)                 ;最后一帧
;;    ("g" . ub-frame-goto)                ;跳到某一帧
;;    ("C-m" . ub-frame-retain-redo)       ;保留重做
;;    ("q" . ub-mode-quit)                 ;退出
;;    ("?" . ub-help)                      ;帮助
;;    ("J" . scroll-up-one-line)           ;向上滚动一行
;;    ("K" . scroll-down-one-line)         ;向下
;;    )
;;  ub-mode-map
;;  )
;; (lazy-set-key vi-move-key-alist ub-mode-map-default ) ;vi-move 的局部按键
;;; ### Calc ###
;;; --- 计算器
(lazy-set-key
 '(
   ("C-x c" . calc)                     ;计算器
   ))                                   ;;; ### Calendar ###
;;; --- 日历
(lazy-set-key
 '(("C-c c" . calendar)))
(lazy-unset-key
 '("a")
 calendar-mode-map)                     ;卸载按键
(lazy-set-key
 '(
   ("j" . calendar-forward-week)              ;下一个星期
   ("k" . calendar-backward-week)             ;上一个星期
   ("l" . calendar-forward-day)               ;下一天
   ("h" . calendar-backward-day)              ;上一天
   ("L" . calendar-forward-month)             ;下一月
   ("H" . calendar-backward-month)            ;上一月
   ("J" . calendar-forward-year)              ;下一年
   ("K" . calendar-backward-year)             ;上一年
   ("aw" . calendar-beginning-of-week)        ;一星期的第一天
   ("ew" . calendar-end-of-week)              ;一星期的最后一天
   ("am" . calendar-beginning-of-month)       ;一月的第一天
   ("em" . calendar-end-of-month)             ;一月的最后一天
   ("ay" . calendar-beginning-of-year)        ;一年的第一天
   ("ey" . calendar-end-of-year)              ;一年的最后一天
   (";" . calendar-goto-today)                ;跳到今天
   ("," . calendar-scroll-left)               ;向左滚动一月
   ("." . calendar-scroll-right)              ;向右滚动一月
   ("<" . calendar-scroll-left-three-months)  ;向左滚动三月
   (">" . calendar-scroll-right-three-months) ;向右滚动三月
   ("q" . calendar-exit)                      ;退出
   )
 calendar-mode-map)
;;; ### Keyboard Macro ###
;;; --- 键盘宏
(lazy-set-key
 '(
   ("M-s-s" . kmacro-start-macro-or-insert-counter) ;开始键盘宏或插入
   ("M-s-d" . kmacro-end-or-call-macro)             ;结束键盘宏或调用
   ("M-s-c" . kmacro-delete-ring-head)              ;删除当前的键盘宏
   ("M-s-w" . kmacro-cycle-ring-next)               ;下一个键盘宏
   ("M-s-e" . kmacro-cycle-ring-previous)           ;上一个键盘宏
   ("M-s-a" . kmacro-edit-macro)                    ;编辑键盘宏
   ("M-s-v" . name-last-kbd-macro)                  ;命令当前键盘宏
   ("M-s-f" . insert-kbd-macro)                     ;插入键盘宏
   ("M-s-q" . apply-macro-to-region-lines)          ;应用键盘宏到选择的区域
   ))
;;; ### Ielm ###
;;; --- Emacs Lisp 解释模式
(lazy-unset-key
 '("M-p" "M-n")
 ielm-map)                              ;卸载按键
(lazy-set-key
 '(
   ("M-s-i" . ielm-toggle)              ;切换ielm
   ))
(lazy-set-key
 '(
   ("C-s-p" . comint-previous-input)    ;上一个输入
   ("C-s-n" . comint-next-input)        ;下一个输入
   )
 ielm-map
 )
;;; ### Go Change ###
;;; --- 修改轨迹
(lazy-set-key
 '(
   ("s-/" . goto-last-change)           ;跳转到最近修改, 向前
   ("s-?" . goto-last-change-reverse)   ;跳转到最近修改, 向后
   ))
;;; ### Archive ###
;;; --- 压缩模式
;; (lazy-set-key
;;  '(
;;    ("j" . archive-next-line)            ;下一行
;;    ("k" . archive-previous-line)        ;上一行
;;    ("C-m" . archive-extract)            ;解压
;;    ("E" . archive-extract-other-window) ;解压道其他窗口
;;    ("m" . archive-mark)                 ;标记
;;    ("d" . archive-flag-deleted)         ;删除标记
;;    ("x" . archive-expunge)              ;擦除有删除标记的文件
;;    ("u" . archive-unflag)               ;解除标记, 向下移动
;;    ("i" . archive-unflag-backwards)     ;解除标记, 并向上移动
;;    ("U" . archive-unmark-all-files)     ;解除所有标记
;;    ("g" . revert-buffer)                ;刷新
;;    ("q" . quit-window)                  ;退出
;;    ("f" . archive-view)                 ;浏览
;;    ("r" . archive-rename-entry)         ;重命令
;;    ("e" . scroll-down)                  ;向下滚动一屏
;;    (" " . scroll-up)                    ;向上滚动一屏
;;    ("M" . archive-chmod-entry)          ;chmod操作
;;    ("G" . archive-chgrp-entry)          ;chgrp操作
;;    ("O" . archive-chown-entry)          ;chown操作
;;    )
;;  archive-mode-map
;;  )
;;; ### Completion List ###
;;; --- 补全列表
(lazy-set-key vi-move-key-alist completion-list-mode-map) ;vi-move的局部按键
(lazy-set-key sdcv-key-alist completion-list-mode-map)    ;sdcv 的局部按键
;; Zencoding
(lazy-set-key
 '(
   ("C-c C-c" . gnome-open-buffer)
   ("C-c C-l" . dired-open-buffer)
   ("RET" . newline)
   ("TAB" . yas/expand)
   ("M-i" . forward-indent)
   )
 html-helper-mode-map
 )
;;; ### coffee-mode ###
(lazy-set-key
 '(
   ("C-m" . comment-indent-new-line)
   )
 coffee-mode-map)
;;; ### Babel ###
;;; --- 网络翻译接口
;; (lazy-set-key
;;  '(
;;    ("s-t" . babel-smart)                ;智能翻译
;;    ))
;; (lazy-set-key
;;  '(
;;    ("q" . babel-quit)                   ;退出
;;    ("," . end-of-buffer)                ;最后面
;;    ("." . beginning-of-buffer)          ;最前面
;;    ("s" . isearch-forward)              ;向前搜索
;;    ("r" . isearch-backward)             ;向后搜索
;;    )
;;  babel-mode-map
;;  )
;; (lazy-set-key vi-move-key-alist babel-mode-map)
;;; ### Breadcrumb ###
;;; --- 书签管理导航
(lazy-set-key
 '(
   ("s-7" . bc-local-next)              ;局部下一个
   ("s-8" . bc-local-previous)          ;局部上一个
   ("s-9" . bc-next)                    ;全局下一个
   ("s-0" . bc-previous)                ;全局上一个
   ("s-o" . bc-goto-current)            ;跳到当前
   ("s-l" . bc-list)                    ;书签列表
   ("s-'" . bc-set)                     ;书签设定
   ))
(lazy-set-key
 '(
   ("j" . next-line)                    ;下一行
   ("k" . previous-line)                ;上一行
   ("d" . bc-menu-mark-delete)          ;标记删除当前
   ("D" . bc-menu-mark-all-delete)      ;标记删除所有
   ("x" . bc-menu-commit-deletions)     ;确认删除
   ("u" . bc-menu-unmark-delete)        ;去标记当前
   ("U" . bc-menu-unmark-all-delete)    ;去标记所有
   ("v" . bc-menu-visit-other)          ;在其他窗口中浏览
   ("f" . bc-menu-jump)                 ;跳到书签处
   )
 *bc-menu-mode-map*
 )
;;; ### Text Translator ###
;;; --- 文本翻译
(lazy-set-key
 '(
   ("s-x ti" . text-translator)                             ;全文翻译, 输入
   ("s-x tt" . text-translator-translate-by-auto-selection) ;全文翻译自动选择
   ))
;;; ### Completion Operation ###
;;; --- 补全操作
(lazy-set-key
 '(
   ;;("C-c l" . semantic-ia-complete-symbol-menu) ;弹出补全菜单
   ("C-c SPC" . senator-completion-menu-popup) ;弹出补全菜单
   ("M-/" . hippie-expand)                     ;智能补全
   ))
;;; ### Golang ###
(lazy-unset-key
 '("C-k" "M-o")
 go-mode-map)
(lazy-set-key
 '(
   ("C-c C-c" . go-run-buffer)
   ("C-c C-f" . gofmt)
   ("C-c C-d" . godoc)
   ("C-c C-a" . go-import-add)
   ("C-8" . godef-jump)
   ("C-u C-8" . godef-jump-other-window)
   ("C-k" . go-kill)
   ("M-o" . go-backward-delete)
   )
 go-mode-map
 )
;;; ### VC ###
;;; --- 版本控制
(lazy-set-key
 '(
   ("C-x v" . one-key-menu-VC)          ;版本控制
   ))
;;; ### Ispell ###
;;; --- 拼写检查
(lazy-set-key
 '(("s-v s-v" . ispell-buffer)))        ;检查当前buffer
;;; ### Slime mode ###
(lazy-set-key
 '(
   ("M-n" . go-to-next-pair-left)
   ("M-p" . go-to-next-pair-right)
   ("C-m" . comment-indent-new-line)
   ("C-c C-l" . slime-load-current-file)
   )
 slime-mode-map
 )
;;; ### Markdown ###
;;; --- Markdown mode
(lazy-set-key
 '(
   ("<tab>" . yas/expand))
 markdown-mode-map
 )

(provide 'LazyCatKeystoke)
