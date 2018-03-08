[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA Stable](https://stable.melpa.org/packages/olivetti-badge.svg)](https://stable.melpa.org/#/olivetti)
[![MELPA](https://melpa.org/packages/olivetti-badge.svg)](https://melpa.org/#/olivetti)

Olivetti is a simple Emacs minor mode for a nice writing environment.

![screenshot](https://github.com/rnkn/olivetti/raw/master/screenshots/01.png)

Pictured: *Big Fish* by John August using using text body width of 66
(top, also using [Fountain Mode]), lorem ipsum text using text body
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
  `olivetti-set-width` <kbd>C-c \ </kbd>
- If `olivetti-body-width` is an integer, the text body width will scale
  with use of `text-scale-mode`, whereas if a fraction (float) then the
  text body width will remain at that fraction.
- Optionally remember the state of `visual-line-mode` on entry and
  recall its state on exit.
- Optionally hide the mode-line for distraction-free writing.

Olivetti keeps everything it does buffer-local, so you can write prose in one
buffer and code in another, side-by-side in the same frame. Or, by hiding the
mode-line and using a single window in a fullscreen frame, Olivetti provides a
nice distraction-free environment. For those looking for a hardcore
distraction-free writing mode with a much larger scope, I recommend
[writeroom-mode].

[writeroom-mode]: https://github.com/joostkremers/writeroom-mode "Writeroom Mode"

Requirements
------------

- Emacs 24.4

Installation
------------

Olivetti is available through [MELPA] and [MELPA-stable]. I
encourage installing the stable version.

Alternately, download the [latest release] and put it in your
`load-path`.

[melpa]: https://melpa.org/ "MELPA"
[melpa-stable]: https://stable.melpa.org/ "MELPA Stable"
[latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

Known Bugs
----------

- `linum-mode` in Emacs versions earlier than 26.1 has a bug that overwrites
  margin settings, making it incompatible with modes that work with margins.
  More information here: <https://debbugs.gnu.org/20674>.

Please report bugs on GitHub [Issues] page.

[issues]: https://github.com/rnkn/olivetti/issues "Olivetti issues"

History
-------

See [Releases].

[releases]: https://github.com/rnkn/olivetti/releases "Olivetti releases"

Hints
-----

To always use a different width for a specific file, set a [File Variable]
specifying `olivetti-body-width`:

    M-x add-file-local-variable RET olivetti-body-width RET 66 RET

[file variable]: https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html "File Variables"
