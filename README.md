Olivetti
========

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
- Interactively change body width with `olivetti-expand`,
  `olivetti-shrink` and `olivetti-set-width`.
- If `olivetti-body-width` is an integer, the text body width will scale
  with use of `text-scale-mode`, whereas if a fraction (float) then the
  text body width will remain at that fraction.
- Optionally hide the modeline for distraction-free writing.

Requirements
------------

- Emacs 24.4

Installation
------------

Olivetti is available through [MELPA][] and [MELPA-stable][]. I
encourage installing the stable version.

Alternately, download the [latest release][] and put it in your
`load-path`.

[melpa]: http://melpa.milkbox.net "MELPA"
[melpa-stable]: http://melpa-stable.milkbox.net "MELPA"
[latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

Known Bugs
----------

- `linum-mode` currently has a bug that overwrites margin settings,
  making it incompatible with Olivetti. More information here:
  <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20674>

Please report bugs on GitHub [Issues][] page.

[issues]: https://github.com/rnkn/olivetti/issues "Olivetti issues"

History
-------

See [Releases][].

[releases]: https://github.com/rnkn/olivetti/releases "Olivetti releases"
