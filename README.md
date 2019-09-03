# Olivetti #

A simple Emacs minor mode for a nice writing environment.

Screenshot: https://f002.backblazeb2.com/file/pwr-share/olivetti.png

## Features ##

- Set a desired text body width to automatically resize window margins to
  keep the text comfortably in the middle of the window.
- Text body width can be the number of characters (an integer) or a fraction
  of the window width (a float between 0.0 and 1.0).
- Interactively change body width with:
  olivetti-shrink C-c { { { ...
  olivetti-expand C-c } } } ...
  olivetti-set-width C-c \
- If olivetti-body-width is an integer, the text body width will scale with
  use of text-scale-mode, whereas if a fraction (float) then the text body
  width will remain at that fraction.
- Optionally remember the state of visual-line-mode on entry and recall its
  state on exit.

Olivetti keeps everything it does buffer-local, so you can write prose in one
buffer and code in another, side-by-side in the same frame. For those looking
for a hardcore distraction-free writing mode with a much larger scope, I
recommend writeroom-mode: https://github.com/joostkremers/writeroom-mode.

## Requirements ##

- Emacs 24.5

## Installation ##

The latest stable release of Olivetti is available via [MELPA-stable]
and can be installed with:

    M-x package-install RET olivetti RET

Alternately, download the [latest release], move this file into your
load-path and add to your .emacs/init.el file:

    (require 'olivetti)

If you prefer the latest but perhaps unstable version, install via
[MELPA], or clone the repository into your load-path and require as
above:

    git clone https://github.com/rnkn/olivetti.git

[melpa]: https://melpa.org/#/olivetti "MELPA"
[melpa-stable]: https://stable.melpa.org/#/olivetti "MELPA-stable"
[latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

## Contributing ##

Please report bugs and request features at
<https://github.com/rnkn/fountain-mode/issues>

## Hints ##

To always use a different width for a specific file, set a File
Variable:

    M-x add-file-local-variable RET olivetti-body-width RET 66 RET

See (info "(emacs) File Variables").
