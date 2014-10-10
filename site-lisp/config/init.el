(with-temp-message ""                   ;抹掉插件启动的输出
  ;; 必须加载的
  (require 'benchmark-init)
  (require 'theme)
  (require 'init-startup)
  (require 'lazy-set-key)
  (require 'one-key)
  (require 'paredit)
  (require 'tabbar)
  (require 'basic-toolkit)
  (require 'redo)
  (require 'highlight-parentheses)

  (require 'init-smex)
  (require 'init-minibuffer-tray)
  (require 'init-linum)
  (require 'init-flymake)
  (require 'init-auto-save)
  (require 'init-jedi)
  (require 'init-tabbar)
  (require 'init-mode)
  (require 'init-dired)
  (require 'init-session)
  (require 'init-paredit)
  (require 'init-indent)
  (require 'init-auto-complete)
  (require 'init-one-key)
  (require 'init-iedit)
  (require 'init-visual-regexp)
  (require 'init-key)
  (require 'init-generic)
  (require 'init-auto-indent-mode)

  ;; 可以延后加载的
  (run-with-idle-timer
   1 t
   #'(lambda ()
       (require 'pretty-lambdada)
       (require 'browse-kill-ring)

       (require 'init-tempbuf)
       (require 'init-doc-view)
       (require 'init-backup)
       (require 'init-eldoc)
       (require 'init-doxymacs)
       (require 'init-yasnippet)
       (require 'init-package)
       (require 'init-smooth-scrolling)
       (require 'init-cursor-chg)
       (require 'init-winpoint)
       (require 'init-benchmark)

       (require 'init-idle)

       ))
  )

(provide 'init)
