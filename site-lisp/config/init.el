(require 'theme)                        ;主题
(require 'scroll-mode-line-mode)        ;滚动信息栏
(require 'thing-edit)                   ;基于thingatpt的编辑扩展
(require 'thing-edit-extension)         ;thing-edit 增强
(require 'paredit)
(require 'tabbar)                       ;多标签
(require 'miniedit)                     ;编辑输入栏
(require 'lazy-set-key)                 ;懒惰按键设置扩展
(require 'basic-edit-toolkit)
(require 'redo)                         ;重做命令
(require 'linum)                        ;显示行号
(require 'paredit-extension)            ;Paredit扩展
(require 'window-extension)
(require 'rect-extension)               ;矩形编辑扩展
(require 'tabbar-extension)             ;Tabbar 的扩展
(require 'lazy-search)                  ;懒惰搜索
(require 'lazy-search-extension)        ;lazy-seach 扩展
(require 'go-to-char)                   ;跳转到某个字符
(require 'buffer-extension)

(require 'init-linum)
(require 'init-paredit)
(require 'init-key)
(require 'init-startup)                 ;启动设置
(require 'init-auto-save)

(eval-after-load "emacs-lisp-mode" '(require 'init-yasnippet))
(eval-after-load "qml-mode" '(require 'init-yasnippet))
(eval-after-load "python-mode" '(require 'init-yasnippet))
(eval-after-load "go-mode" '(require 'init-yasnippet))
(eval-after-load "html-mode" '(require 'init-yasnippet))

(provide 'init)
