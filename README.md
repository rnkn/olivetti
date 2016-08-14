Olivetti
========

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA Stable](http://stable.melpa.org/packages/olivetti-badge.svg)](http://stable.melpa.org/#/olivetti)
[![MELPA](http://melpa.org/packages/olivetti-badge.svg)](http://melpa.org/#/olivetti)

Olivetti is a simple Emacs minor mode for a nice writing environment.

![screenshot](http://files.paulwrankin.com/olivetti/screenshot.png)

Pictured: *Big Fish* by John August using using text body width of 66
(top, also using [Fountain Mode][]), lorem ipsum text using text body
width of 80 (bottom)

[fountain mode]: https://github.com/rnkn/fountain-mode

Features
--------

- Set a desired text body width to automatically resize window margins
  to keep the text comfortably in the middle of the window.
- Text body width can be the number of characters (an integer) or a
  fraction of the window width (a float between 0.0 and 1.0).
- Interactively change body width with:  
  `olivetti-shrink` <kbd>C-c [</kbd> <kbd>[</kbd> <kbd>[</kbd> ...  
  `olivetti-expand` <kbd>C-c ]</kbd> <kbd>]</kbd> <kbd>]</kbd> ...  
  `olivetti-set-width` <kbd>C-c \\</kbd>
- If `olivetti-body-width` is an integer, the text body width will scale
  with use of `text-scale-mode`, whereas if a fraction (float) then the
  text body width will remain at that fraction.
- Optionally remember the state of `visual-line-mode` on entry and
  recall its state on exit.
- Optionally hide the mode-line for distraction-free writing.

Requirements
------------

- Emacs 24.4

Installation
------------

Olivetti is available through [MELPA][] and [MELPA-stable][]. I
encourage installing the stable version.

Alternately, download the [latest release][] and put it in your
`load-path`.

[melpa]: https://melpa.org/ "MELPA"
[melpa-stable]: https://stable.melpa.org/ "MELPA"
[latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

Known Bugs
----------

- `linum-mode` currently has a bug that overwrites margin settings,
  making it incompatible with Olivetti. More information here:
  <http://debbugs.gnu.org/20674>.
- Emacs 25.1 introduced changes to `window-min-width` that return
  erroneously large minimum window widths when using large window
  margins, causing `split-window-right` to fail with a misleading
  error message. This necessitates Olivetti patching `split-window-right`
  to always split the window in half (in line with its documentation
  string). This is designed as a temporary workaround until the Emacs
  maintainers fix the problems with `window-min-width`.
  See <http://debbugs.gnu.org/24193>.

Please report bugs on GitHub [Issues][] page.

[issues]: https://github.com/rnkn/olivetti/issues "Olivetti issues"

History
-------

See [Releases][].

[releases]: https://github.com/rnkn/olivetti/releases "Olivetti releases"
