c-eldoc.el
==========

Display description of the function under the cursor.

![screenshot of c-eldoc](http://github.com/mooz/c-eldoc/raw/master/images/screenshot.png"Display prototypes for the function under the cursor")

Original version: [http://www.emacswiki.org/emacs/c-eldoc.el](http://www.emacswiki.org/emacs/c-eldoc.el)

This forked version uses [deferred.el](http://github.com/kiwanami/emacs-deferred "emacs-deferred") to support asynchronous processing.

Installation
============

First, download and compile `c-eldoc.el`

    wget http://github.com/mooz/c-eldoc/raw/master/c-eldoc.el

If you havn't installed `deferred.el`,

    wget http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el

Then, place `c-eldoc.el` (and `deferred.el`) into your site-lisp directory.

Finally, add following lines to your emacs config file (e.g. `.emacs`).

    (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
    (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)

Customization
=============

Cache
-----

`c-eldoc.el` uses cache to reduce the times of compilation. This cache has expiration time (default 120 seconds) and you can customize this value.

    (setq c-eldoc-buffer-regenerate-time 60)

You can explicitly force update the cache and get latest result by calling `c-eldoc-force-cache-update`.

The following settings this update command to the `Ctrl-c + d` key.

    (defun c-eldoc-define-keybindings (map)
      (define-key map (kbd "C-c d") 'c-eldoc-force-cache-update))
    
    (add-hook 'c-mode-hook
              (lambda ()
                (c-eldoc-define-keybindings c-mode-map)))
    
    (add-hook 'c++-mode-hook
              (lambda ()
                (c-eldoc-define-keybindings c++-mode-map)))

Compile options
---------------

You can customize the compiler and the compile options.

Here are the default values.

    (defvar c-eldoc-cpp-command "/lib/cpp ") ;; compiler
    (defvar c-eldoc-cpp-macro-arguments "-dD -w -P")
    (defvar c-eldoc-cpp-normal-arguments "-w -P")
    (defvar c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ") ;; include flags

To customize them, use `setq`.

    (setq c-eldoc-cpp-command "/usr/local/bin/clang")

Styles
-----------------------------

You can customize style of current argument.

Press `M-x` and type `customize-face` and specify `c-eldoc-current-argument-face`.
