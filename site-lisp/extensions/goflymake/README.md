## Emacs flymake-mode for the Go programming language

The `goflymake` program is a wrapper around the `go` tool to provide
Emacs flymake style syntax checking for golang source files within
multi-file packages and _test.go files.  Support for os/arch specific
*cgo* files is included thanks to the standard *go/build* package.

### Setup

 1. If needed, update your **${PATH}** to include Go installed binaries, for example:

    `export PATH=${PATH}:${GOPATH}/bin`

    Depending on your Emacs workflow (e.g., windowing system environment), it may be required to explicitly set
the following items:
    1. ``(setenv "GOPATH" "/path/to/gopath")``
    2. ``(setenv "PATH" (concat (getenv "PATH") ":" "/extra/path/element"))``
    3. ``(setq exec-path (append exec-path (list (expand-file-name "/another/thing"))))``

 2. Install goflymake:

    `go get -u github.com/dougm/goflymake`

### Emacs setup

 1. Install go-mode.el if you haven't already

 2. Add these lines to your **.emacs** or similar:

   * **flymake**

            (add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
            (require 'go-flymake)

   * **flycheck**

            (add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
            (require 'go-flycheck)


### ToDo

We probably shouldn't need the `goflymake` program, the `go` tool could
be tweaked to support the flymake style of syntax checking.
Maybe there is already a better way, but I couldn't find one.

### Troubleshooting

The ``goflymake`` command includes forensic information to assist in debugging
anomalies, which will assist you in tracking down the problem.

If worst comes to worst, the [Flymake Troubleshooting Guide](http://www.gnu.org/software/emacs/manual/html_node/flymake/Troubleshooting.html)
is definitely helpful.
