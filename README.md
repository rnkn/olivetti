Olivetti
========

[![MELPA Stable](https://stable.melpa.org/packages/olivetti-badge.svg)][1]
[![MELPA](https://melpa.org/packages/olivetti-badge.svg)][2]

A simple Emacs minor mode for a nice writing environment.

![olivetti screenshot](screenshot.png)


Features
--------

 - Set a desired text body width to automatically resize window margins
   to keep the text comfortably in the middle of the window.
 - Text body width can be the number of characters (an integer), a fraction of
   the window width (a float between 0.0 and 1.0), or `nil` which uses the value
   of `fill-column` +2.
 - Interactively change body width with:  
   `olivetti-shrink C-c { { { ...`  
   `olivetti-expand C-c } } } ...`  
   `olivetti-set-width C-c |`  
 - If `olivetti-body-width` is an integer, the text body width will
   scale with use of text-scale-mode, whereas if a fraction (float) then
   the text body width will remain at that fraction.
 - Change the way the text body margins look with option `olivetti-style`: use
   margins, fringes, or both for a fancy "page" look.
 - Customize `olivetti-fringe` face to affect only Olivetti buffers.
 - Optionally remember the state of `visual-line-mode` on entry and
   recall its state on exit.

Olivetti keeps everything it does buffer-local, so you can write prose
in one buffer and code in another, side-by-side in the same frame.


Requirements
------------

 - Emacs 24.4


Installation
------------

The latest stable release of Olivetti is available via
[MELPA-stable][1]. First, add MELPA-stable to your package archives:

    M-x customize-option RET package-archives RET
    
Insert an entry named `melpa-stable` with URL:
`https://stable.melpa.org/packages/`

You can then find the latest stable version of `olivetti` in the
list returned by:

    M-x list-packages RET

If you prefer the latest but perhaps unstable version, do the above
using [MELPA][2].


Advanced Installation
---------------------

Download the latest tagged release, move this file into your `load-path`
and add to your `init.el` file:

    (require 'olivetti)

If you wish to contribute to or alter Olivetti's code, clone the
repository into your load-path and require as above:

    git clone https://github.com/rnkn/olivetti.git


Bugs and Feature Requests
-------------------------

Send me an email (address in the package header). For bugs, please
ensure you can reproduce with:

    $ emacs -Q -l olivetti.el


Hints
-----

To always use a different width for a specific file, set a File
Variable:

    M-x add-file-local-variable RET olivetti-body-width RET 66 RET

See `(info "(emacs) File Variables")`


Alternatives
------------

For those looking for a hardcore distraction-free writing mode with a much
larger scope, I recommend [Writeroom Mode](https://github.com/joostkremers/writeroom-mode).


[1]: https://stable.melpa.org/#/olivetti
[2]: https://melpa.org/#/olivetti
