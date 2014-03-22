# Ruby tools
Ruby tools is a collection of handy functions for Emacs
`ruby-mode`. You can turn a string to symbol, symbol to string, single
to double quote string, double to single quote string, clear string,
interpolate and more...

[<img src="http://img.youtube.com/vi/xYMcVpTp4uY/0.jpg">](https://www.youtube.com/watch?v=xYMcVpTp4uY)

## Installation
I recommend installing via ELPA, but manual installation is simple as well:

    (add-to-list 'load-path "/path/to/ruby-tools")
    (require 'ruby-tools)

## Usage
When `ruby-mode` is started, `ruby-tools-mode` will automatically start.

Once started, this functionality is available:

### Convert symbol to string

Insert:

    foo(:bar)

Place cursor on `:bar`, then press `C-'` and you will see:

    foo('bar')

### Convert string to symbol

Insert:

    foo('bar')
    foo("bar")

Place cursor on `bar`, then press `C-:` and you will see:

    foo(:bar)
    foo(:bar)

### Convert single quote string to double quote string

Insert:

    foo('bar')

Place cursor on `bar`, then press `C-"` and you will see:

    foo("bar")

### Convert double quote string to single quote string

Insert:

    foo("bar")

Place cursor on `bar`, then press `C-'` and you will see:

    foo('bar')

### Clear string content

Insert:

    foo('bar')

Place cursor on `bar`, then press `C-;` and you will see:

    foo('')

### String interpolation

Insert:

    foo('bar')
    foo("bar")
    `bar`
    %(bar)

Place cursor on `bar`, then press `#` and you will see:

    foo('b#ar')
    foo("b#{}ar")
    `b#{}ar`
    %(b#{}ar)

## TODO

* Add/remove parenthesis on function call/definition
* `re-builder` for Ruby regex syntax

## Contribution
Contribution is much welcome! Ruby tools is tested using [Ecukes](http://ecukes.info). When
adding new features, please write tests for them!

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

    $ cd /path/to/ruby-tools
    $ cask

Run all tests with:

    $ make
