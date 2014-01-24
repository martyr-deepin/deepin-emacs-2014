# smooth-scrolling.el

## About

This package is a version of smooth-scrolling.el originally written by
[Adam Spiers](http://adamspiers.org/) that has been modified
to be loaded from package.el.

Make emacs scroll smoothly, keeping the point away from the top and
bottom of the current buffer's window in order to keep lines of
context around the point visible as much as possible, whilst
avoiding sudden scroll jumps which are visually confusing.

This is a nice alternative to all the native <tt>scroll-*</tt> custom
variables, which unfortunately cannot provide this functionality
perfectly.  <tt>scroll-margin</tt> comes closest, but has some bugs
(e.g. with handling of mouse clicks).  See
[Smooth Scrolling](http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling)
for the gory details.

## Installation

Put somewhere on your <tt>load-path</tt> and include
```
   (require 'smooth-scrolling)
```
 in your initialization file.
 
## Notes
This only affects the behaviour of the <tt>next-line</tt> and
<tt>previous-line</tt> functions, usually bound to the cursor keys and
C-n/C-p, and repeated isearches (<tt>isearch-repeat</tt>).  Other methods
of moving the point will behave as normal according to the standard
custom variables.

Prefix arguments to <tt>next-line</tt> and <tt>previous-line</tt> are
honored. The minimum number of lines are scrolled in order to keep the
point outside the margin.

There is one case where moving the point in this fashion may cause
a jump: if the point is placed inside one of the margins by another
method (e.g. left mouse click, or <tt>M-x goto-line</tt>) and then moved in
the normal way, the advice code will scroll the minimum number of
lines in order to keep the point outside the margin.  This jump may
cause some slight confusion at first, but hopefully it is justified
by the benefit of automatically ensuring <tt>smooth-scroll-margin</tt>
lines of context are visible around the point as often as possible.
