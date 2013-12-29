;;; ### Unset key ###
;;; --- 卸载按键
(lazy-unset-key                         ;全局按键的卸载
 '("C-x C-f" "C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c" "C-\\"))
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
;;; ### Toolkit ###
;;; --- 工具函数
(lazy-set-key
 '(
   ("C-x C-c" . checkdoc)                   ;检查文档
   ("C-c ns" . notes-search)                ;便签搜索
   ("C-c nn" . notes-new)                   ;新建便签
   ("s-c o" . one-key-menu-directory)       ;目录打开菜单
   ("s-*" . one-key-menu-backup-file)       ;备份资料
   ("s-," . bury-buffer)                    ;隐藏当前buffer
   ("s-." . unbury-buffer)                  ;反隐藏当前buffer
   ("s-&" . killall)                        ;杀掉进程
   ("C-x w" . count-words)                  ;计算单词的数量
   ("C-z l" . linum-mode)                   ;行号模式切换
   ("s-Z" . dot-emacs)                      ;打开dot-emacs文件
   ("C-x f" . find-file-at-point)           ;文件跳转
   ("s-f" . find-file-root)                 ;用root打开文件
   ("s-r" . find-file-smb)                  ;访问samba
   ("<print>" . save-screenshots)           ;截屏
   ("<M-s-return>" . toggle-debug-on-error) ;切换调试模式
   ("s-R" . re-builder)                     ;可视化构建正则表达式
   ("s-1" . sort-lines)                     ;排序
   ("s-2" . elisp-depend-insert-comment)    ;插入 `...' 注释代码
   ("s-3" . hanconvert-region)              ;转换简体或繁体中文
   ("s-4" . uniquify-all-lines-buffer)      ;删除重复的行
   ("s-5" . elisp-depend-insert-require)    ;插入 (require '...) 语句
   ("s-[" . eval-expression)                ;执行表达式
   ("s-\\" . artist-mode)                   ;绘制模式
   ("M-s-u" . ediff-buffers)                ;ediff
   ("C-s-q" . quoted-insert)                ;读取系一个输入字符并插入
   ))
;;; ### Color-moccur ###
;;; --- 增强的moccur
(lazy-set-autoload-key
 '(
   ("s-x v" . moccur-grep)              ;搜索当前目录下的文件
   )
 "init-moccur")
(lazy-set-autoload-key
 '(
   ("s-x g" . moccur-grep-find-pwd)     ;递归搜索当前目录下的文件
   )
 "dired-extension")
;;; ### Buffer Move ###
;;; --- 缓存移动
(lazy-set-key
 '(
   ("s-J" . scroll-up-one-line)            ;向上滚动一行
   ("s-K" . scroll-down-one-line)          ;向下滚动一行
   ("M-J" . scroll-other-window)           ;向下滚动其他窗口
   ("M-K" . scroll-other-window-down)      ;向上滚动其他窗口
   ("M-<" . scroll-other-window-up-line)   ;向下滚动其他窗口一行
   ("M->" . scroll-other-window-down-line) ;向上滚动其他窗口一行
   ("C-o" . open-newline-above)            ;在上面一行新建一行
   ("C-l" . open-newline-below)            ;在下面一行新建一行
   ("C-z k" . beginning-of-buffer)         ;缓存开始
   ("C-z j" . end-of-buffer)               ;缓存结尾
   ("M-p" . go-to-next-pair-right)         ;在( ),' ', " ", [ ], { }中跳到匹配符号的右边
   ("M-n" . go-to-next-pair-left)          ;在( ), ' ', " ", [ ], { }中跳到匹配符号的左边
   ("%" . match-paren)                     ;当在括号上按 % 时, 自动跳转到与当前括号匹配的另一个括号
   ("s-g" . goto-percent)                  ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-g" . goto-line)                     ;到指定行
   ("M-G" . goto-column)                   ;到指定列
   ("C-M-f" . forward-paragraph)           ;下一个段落
   ("C-M-b" . backward-paragraph)          ;上一个段落
   ("C-M-y" . backward-up-list)            ;向左跳出 LIST
   ("C-M-o" . up-list)                     ;向右跳出 LIST
   ("C-M-u" . backward-down-list)          ;向左跳进 LIST
   ("C-M-i" . down-list)                   ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)          ;函数开头
   ("C-M-e" . end-of-defun)                ;函数末尾
   ("C->" . remember-init)                 ;记忆初始函数
   ("C-<" . remember-jump)                 ;记忆跳转函数
   ("M-s-," . point-stack-pop)             ;buffer索引跳转
   ("M-s-." . point-stack-push)            ;buffer索引标记
   ("s-{" . current-line-move-to-top)      ;移动当前行到最上面一行
   ))
(lazy-set-autoload-key
 '(
   ("M-s" . lazy-search-menu)           ;懒惰搜索
   )
 "lazy-search")
(lazy-set-autoload-key
 '(
   ("C-c j" . go-to-char-forward)       ;快速字母导航, 向前
   ("C-c k" . go-to-char-backward)      ;快速字母导航, 向后
   )
 "go-to-char")
(lazy-set-autoload-key
 '(
   ("s-N" . move-text-down)             ;把光标所在的整行文字(或标记)下移一行
   ("s-P" . move-text-up)               ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")
;;; ### Buffer Name ###
;;; --- 缓存名字
(lazy-set-autoload-key
 '(
   ("s-c r" . rename-file-and-buffer)                 ;更改当前文件的名字
   ("s-c g" . move-buffer-file)                       ;更改当前文件的目录
   ("s-c n" . copy-buffer-file-name-as-kill)          ;拷贝buffer名字
   ("C-M-;" . kill-other-window-buffer)               ;关闭其他窗口的buffer
   ("s-Q" . kill-current-mode-buffers)                ;关闭与当前模式相同的所有buffers
   ("s-q" . kill-current-mode-buffers-except-current) ;关闭当前模式所有buffers, 除了当前buffer
   )
 "buffer-extension")
;;; ### Buffer Edit ###
;;; --- 缓存编辑
(lazy-set-key
 '(
   ("M-N" . kill-syntax-backward+)            ;向后进行语法删除
   ("M-M" . kill-syntax-forward+)             ;向前进行语法删除
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行
   ("C-s-n" . comment-dwim-next-line)         ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)         ;移动到下一行并注释
   ("M-s-n" . comment-part-move-down)         ;向下移动注释
   ("M-s-p" . comment-part-move-up)           ;向上移动注释
   ("C-x C-x" . exchange-point-and-mark)      ;交换当前点和标记点
   ("M-o" . backward-delete-char-untabify)    ;向前删除字符
   ("M-z" . zap-to-char)                      ;向前删除到第一个相符的字符
   ("C-M-z" . zap-back-to-char)               ;向后删除到第一个相符的字符
   ("C-/" . undo)                             ;撤销
   ("C-?" . redo)                             ;重做
   ("s-k" . kill-and-join-forward)            ;在缩进的行之间删除
   ("C-x u" . mark-line)                      ;选中整行
   ("C-M-S-h" . mark-paragraph)               ;选中段落
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("M-SPC" . just-one-space)                 ;只有一个空格在光标处
   ))
;;; ### Rect ###
;;; --- 矩形操作
(lazy-set-key
 '(
   ("s-M" . rm-set-mark)                 ;矩形标记
   ("s-X" . rm-exchange-point-and-mark)  ;矩形对角交换
   ("s-D" . rm-kill-region)              ;矩形删除
   ("s-S" . rm-kill-ring-save)           ;矩形保存
   ("s-Y" . yank-rectangle)              ;粘帖矩形
   ("s-O" . open-rectangle)              ;用空白填充矩形, 并向右移动文本
   ("s-C" . clear-rectangle)             ;清空矩形
   ("s-T" . string-rectangle)            ;用字符串替代矩形的每一行
   ("s-I" . string-insert-rectangle)     ;插入字符串在矩形的每一行
   ("s-F" . delete-whitespace-rectangle) ;删除矩形中空格
   ("s-\"" . copy-rectangle-to-register) ;拷贝矩形到寄存器
   ))
(lazy-set-autoload-key
 '(
   ("s-:" . mark-rectangle-to-end)      ;标记矩形到行末
   )
 "rect-extension")
;;; ### Font ###
;;; --- 字体命令
(lazy-set-key
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增加字体大小
   ("M--" . text-scale-decrease-global) ;减少字体大小, 全局
   ("M-+" . text-scale-increase-global) ;增加字体大小, 全局
   ("M-=" . text-scale-default-global)  ;恢复字体大小, 全局
   ))
;;; ### Window Operation ###
;;; --- 窗口操作
(lazy-set-key
 '(
   ("C-c v" . split-window-vertically)      ;纵向分割窗口
   ("C-c h" . split-window-horizontally)    ;横向分割窗口
   ("C-;" . kill-this-buffer)               ;关闭当前buffer
   ("C-x ;" . delete-other-windows)         ;关闭其它窗口
   ("s-;" . one-key-menu-window-navigation) ;快速窗口导航
   ("s-a" . window-number-jump)             ;窗口快速选择
   ))
(lazy-set-autoload-key
 '(
   ("C-c V" . delete-other-windows-vertically+)   ;关闭上下的其他窗口
   ("C-c H" . delete-other-windows-horizontally+) ;关闭左右的其他窗口
   ("C-'" . delete-current-buffer-and-window)     ;关闭当前buffer, 并关闭窗口
   ("C-\"" . delete-current-buffer-window)        ;删除当前buffer的窗口
   ("C-s-7" . select-next-window)                 ;选择下一个窗口
   ("C-s-8" . select-prev-window)                 ;选择上一个窗口
   ("M-s-o" . toggle-one-window)                  ;切换一个窗口
   )
 "window-extension")
;;; ### Tabbar ###
;;; --- 多标签浏览
(lazy-set-key
 '(
   ("M-7" . tabbar-backward-tab)        ;移动到后一个标签
   ("M-8" . tabbar-forward-tab)         ;移动到前一个标签
   ("M-9" . tabbar-backward-group)      ;移动到后一个标签组
   ("M-0" . tabbar-forward-group)       ;移动到前一个标签组
   ))
(lazy-set-autoload-key
 '(
   ("M-&" . tabbar-backward-tab-other-window) ;向前移动其他窗口的标签
   ("M-*" . tabbar-forward-tab-other-window)  ;向后移动其他窗口的标签
   ("M-s-7" . tabbar-select-beg-tab)          ;移动到最左边的标签
   ("M-s-8" . tabbar-select-end-tab)          ;移动到最右边的标签
   )
 "tabbar-extension")
;;; ### Functin key ###
;;; --- 功能函数
(lazy-set-key
 '(
   ("<f1>" . sh-show-help)                       ;elisp help
   ("<f2>" . refresh-file)                       ;自动刷新文件
   ("<f3>" . visit-tags-table)                   ;查找TAGS文件 (更新TAGS表)
   ("<f4>" . generate-gtags-files)               ;生成gtags引用文件
   ("<f5>" . emacs-session-save)                 ;退出emacs
   ("<f6>" . lock-screen)                        ;锁屏
   ("<f7>" . one-key-menu-ui)                    ;用户界面菜单
   ("<f8>" . dired-jump)                         ;文件管理起
   ("<f9>" . list-load-path-shadows)             ;显示重复加载的库
   ("<f10>" . open-current-log-keyboard-command) ;打开命令日志
   ("<f11>" . fullscreen-toggle)                 ;全屏切换
   ("<f12>" . hibernate-disk)                    ;休眠
   ("M-1" . strip-blank-lines)                   ;删除选中区域的所有空行
   ("M-2" . indent-buffer)                       ;自动格式化当前Buffer
   ("M-3" . delete-trailing-whitespace)          ;删除行末空格
   ("M-4" . whitespace-cleanup)                  ;清理空格
   ("M-5" . insert-line-number+)                 ;自动在行首添加行号
   ("M-6" . strip-line-number)                   ;删除选中区域的行号
   ("C-4" . insert-changelog-date)               ;插入日志时间 (%Y/%m/%d)
   ("C-5" . insert-standard-date)                ;插入标准时间 (%Y-%m-%d %T)
   ("C-&" . switch-to-messages)                  ;跳转到 *Messages* buffer
   ("C-7" . jump-back)                           ;返回查找符号定义前的位置
   ("M-I" . backward-indent)                     ;向后移动4个字符
   ))
(lazy-set-autoload-key
 '(
   ("C-8" . find-function-or-variable-at-point) ;查找符号的定义
   )
 "find-func-extension")
;;; ### Paredit ###
;;; --- 结构化编程
(lazy-unset-key
 '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b")
 paredit-mode-map)                      ;卸载按键
(lazy-set-key
 '(
   ;; 符号插入
   ("(" . paredit-open-parenthesis)           ;智能 (
   (")" . paredit-close-parenthesis)          ;智能 )
   ("[" . paredit-open-bracket)               ;智能 [
   ("]" . paredit-close-bracket)              ;智能 ]
   ("{" . paredit-open-curly)                 ;智能 {
   ("}" . paredit-close-curly)                ;智能 }
   ("C-s-," . paredit-open-angled)            ;智能 <
   ("C-s-." . paredit-close-angled)           ;智能 >
   ("\"" . paredit-doublequote)               ;智能 "
   ("\\" . paredit-backslash)                 ;智能 \
   ;; 删除
   ("M-o" . paredit-backward-delete)          ;向后删除
   ("C-d" . paredit-forward-delete)           ;向前删除
   ("C-k" . paredit-kill)                     ;删除
   ("C-M-m" . paredit-forward-kill-word)      ;向前按词删除
   ("C-M-n" . paredit-backward-kill-word)     ;向后按词删除
   ;; 移动
   ("C-M-S-m" . paredit-forward)              ;向前移动
   ("C-M-S-n" . paredit-backward)             ;向后移动
   ;; 包围
   ("M-\"" . paredit-meta-doublequote)        ;用 " " 包围对象, 或跳出字符串
   ("M-(" . paredit-wrap-sexp)                ;用 ( ) 包围当前对象
   ("M-[" . paredit-wrap-square)              ;用 [ ] 包围对象
   ("M-{" . paredit-wrap-curly)               ;用 { } 包围对象
   ("C-(" . paredit-wrap-angled)              ;用 < > 包围对象
   ;; 跳出并换行缩进
   ("M-}" . paredit-close-curly-and-newline)  ;跳出 { } 并换行
   ("M-]" . paredit-close-square-and-newline) ;跳出 [ ] 并换行
   ("C-)" . paredit-close-angled-and-newline) ;跳出 < > 并换行
   ;; 其他
   ("C-j" . paredit-newline)            ;智能换行并缩进
   ("M-q" . paredit-reindent-defun)     ;重新格式化函数
   ("M-s-r" . paredit-raise-sexp)       ;提取表达式, 并删除同一等级的其他表达式
   ("M-s-b" . paredit-convolute-sexp)   ;嵌套表达式
   ;; ("M-s-'" . one-key-menu-paredit)     ;Paredit 菜单
   )
 paredit-mode-map
 )
(lazy-set-autoload-key
 '(
   ("C-M-:" . paredit-comment-list-and-newline) ;注释当前LIST并换行
   ("M-:" . paredit-close-round-and-newline+)   ;跳出 ( ) 或 " " 并换行
   ("M-?" . paredit-forward-sexp-and-newline)   ;移动到下一个表达式, 并换行
   ("M-)" . paredit-splice-sexp+)               ;去除包围对象的括号, 并删除空行
   )
 "paredit-extension")
;;; ### Thingh-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-set-autoload-key
 '(
   ("M-s-h" . one-key-menu-thing-edit)  ;thing-edit 菜单
   )
 "init-thing-edit"
 )
;;; ### Multi-Term ###
;;; --- 多标签式的shell
(lazy-set-autoload-key
 '(
   ("s-e" . multi-term-next)                 ;下一个终端
   ("s-w" . multi-term-prev)                 ;上一个终端
   ("s-n" . multi-term)                      ;新建一个终端
   ("s-x s-x" . multi-term-dedicated-toggle) ;切换专注终端
   ("s-x s-z" . multi-term-dedicated-select) ;选择专注终端
   )
 "init-multiterm")
;;; ### W3m ###
;;; --- 网页浏览器
(lazy-set-autoload-key
 '(
   ("C-z C-z" . w3m)                    ;启动W3M
   ("s-W" . one-key-menu-w3m-search)    ;w3m 搜索菜单
   )
 "init-w3m")
(lazy-set-autoload-key
 '(
   ("C-z z" . w3m-startup-background)         ;启动W3M, 后台
   ("C-x C-z" . toggle-w3m-with-other-buffer) ;在W3M和buffer间切换
   )
 "w3m-extension")
;;; ### Dired ###
;;; --- Dired
(lazy-set-autoload-key
 '(
   ("<f8>" . dired-jump)
   ("C-x C-f" . find-file)
   )
 "init-dired")

;;; ### Anything ###
;;; --- 快速buffer切换
(lazy-set-autoload-key
 '(("s-y" . anything)                   ;anything
   ("C-s-y" . anything-call-source)     ;调用特定的源
   )
 "init-anything")

(lazy-set-autoload-key
 '(
   ("M-C" . one-key-menu-cycle-buffer)  ;特定模式切换
   )
 "init-cycle-buffer")
;;; ### Isearch ###
;;; --- 交互式搜索
(lazy-set-key
 '(
   ("TAB" . isearch-complete)               ;isearch补全
   ("C-s" . isearch-repeat-forward)         ;重复向前搜索, 第一次可以用来搜索上一次的历史哟
   ("C-r" . isearch-repeat-backward)        ;重复向后搜索
   ("C-g" . isearch-abort)                  ;中止搜索
   ("C-w" . isearch-yank-word-or-char)      ;粘帖光标后的词或字符作为搜索对象
   ("C-y" . isearch-yank-line)              ;粘帖光标后的行作为搜索对象
   ("M-o" . isearch-delete-char)            ;删除
   ("M-p" . isearch-ring-retreat)           ;搜索历史向后
   ("M-n" . isearch-ring-adjust)            ;搜索历史向前
   ("M-y" . isearch-yank-kill)              ;从 kill ring 中粘帖最后一项到搜索对象后
   ("M-h" . isearch-yank-char)              ;粘帖光标后的字符到搜索对象
   ("M-e" . isearch-edit-string)            ;编辑搜索对象
   ("M-c" . isearch-toggle-case-fold)       ;切换大小写
   ("M-r" . isearch-toggle-regexp)          ;切换正则表达式
   ("M-w" . isearch-toggle-word)            ;切换词
   ("M-g" . isearch-moccur)                 ;moccur 当前 buffer
   ("M-G" . isearch-moccur-all)             ;moccur 所有 buffer
   ("M->" . isearch-beginning-of-buffer)    ;跳转到buffer开头并重新搜索, 搜索最前面一个
   ("M-<" . isearch-end-of-buffer)          ;跳转到buffer末尾并重新搜索, 搜索最后面一个
   ("M-%" . isearch-query-replace)          ;替换
   ("M-d" . isearch-find-duplicate-word)    ;查找重复的单词
   ("M-z" . isearch-find-duplicate-line)    ;查找重复的行
   ("C-M-%" . isearch-query-replace-regexp) ;正则表达式替换
   )
 isearch-mode-map
 )
(lazy-set-autoload-key
 '(
   ("M-l" . isearch-to-lazy-search)     ;切换到lazy-search
   )
 "lazy-search")
;;; ### Help ###
;;; --- 帮助模式
(eval-after-load 'help-mode
  '(progn
     (lazy-set-key
      '(
        ("J" . scroll-up-one-line)      ;向下滚动一行
        ("K" . scroll-down-one-line)    ;向上滚动一行
        ("H" . describe-mode)           ;帮助
        ("f" . help-go-forward)         ;前一个帮助
        ("b" . help-go-back)            ;后一个帮助
        ("y" . sdcv-search-pointer+)    ;翻译
        ("<tab>" . forward-button)      ;前一个按钮
        )
      help-mode-map)
     (lazy-set-key vi-move-key-alist help-mode-map)
     ))
;;; ### Apt-utils ###
;;; --- Apt 管理工具
(lazy-set-autoload-key
 '(
   ("s-x z" . apt-utils-search)         ;APT搜索
   )
 "init-apt-utils")
;;; ### Auto-complete ###
;;; --- 自动补全
(lazy-set-key
 '(
   ("M-h" . ac-complete)                ;补全当前选中的
   ("M-H" . ac-expand-common)           ;补全公共部分
   ("M-U" . ac-stop)                    ;停止
   ("M-," . ac-next)                    ;下一个
   ("M-." . ac-previous)                ;上一个
   ("M-s" . ac-isearch)                 ;搜索补全
   )
 ac-complete-mode-map
 )
;; (lazy-set-key
;;  '(
;;    ("M-/" . auto-complete)))

(provide 'init-key)
