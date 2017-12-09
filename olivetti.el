;;; olivetti.el --- Minor mode for a nice writing environment -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2017 Paul Rankin

;; Author: Paul Rankin <hello@paulwrankin.com>
;; Keywords: wp
;; Version: 1.5.9
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/rnkn/olivetti

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Olivetti is a simple Emacs minor mode for a nice writing environment.

;; Features
;; --------

;; - Set a desired text body width to automatically resize window margins
;;   to keep the text comfortably in the middle of the window.
;; - Text body width can be the number of characters (an integer) or a
;;   fraction of the window width (a float between 0.0 and 1.0).
;; - Interactively change body width with:
;;   `olivetti-shrink` C-c [ [ [ ...
;;   `olivetti-expand` C-c ] ] ] ...
;;   `olivetti-set-width` C-c \
;; - If `olivetti-body-width` is an integer, the text body width will scale
;;   with use of `text-scale-mode`, whereas if a fraction (float) then the
;;   text body width will remain at that fraction.
;; - Optionally remember the state of `visual-line-mode` on entry and
;;   recall its state on exit.
;; - Optionally hide the mode-line for distraction-free writing.

;; Olivetti keeps everything it does buffer-local, so you can write prose in one
;; buffer and code in another, side-by-side in the same frame. Or, by hiding the
;; mode-line and using a single window in a fullscreen frame, Olivetti provides a
;; nice distraction-free environment. For those looking for a hardcore
;; distraction-free writing mode with a much larger scope, I recommend
;; [writeroom-mode].

;; [writeroom-mode]: https://github.com/joostkremers/writeroom-mode "Writeroom Mode"

;; Requirements
;; ------------

;; - Emacs 24.4

;; Installation
;; ------------

;; Olivetti is available through [MELPA] and [MELPA-stable]. I
;; encourage installing the stable version.

;; Alternately, download the [latest release] and put it in your
;; `load-path`.

;; [melpa]: https://melpa.org/ "MELPA"
;; [melpa-stable]: https://stable.melpa.org/ "MELPA Stable"
;; [latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

;; Known Bugs
;; ----------

;; - `linum-mode` in Emacs versions earlier than 26.1 has a bug that overwrites
;;   margin settings, making it incompatible with modes that work with margins.
;;   More information here: <https://debbugs.gnu.org/20674>.

;; Please report bugs on GitHub [Issues] page.

;; [issues]: https://github.com/rnkn/olivetti/issues "Olivetti issues"

;; History
;; -------

;; See [Releases].

;; [releases]: https://github.com/rnkn/olivetti/releases "Olivetti releases"

;; Hints
;; -----

;; To always use a different width for a specific file, set a [File Variable]
;; specifying `olivetti-body-width`:

;;     M-x add-file-local-variable RET olivetti-body-width RET 66 RET

;; [file variable]: https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html "File Variables"


;;; Code:

(defgroup olivetti ()
  "Minor mode for a nice writing environment"
  :prefix "olivetti-"
  :group 'wp)


;;; Variables

(defvar-local olivetti--visual-line-mode
  nil
  "Non-nil if `visual-line-mode' is active when `olivetti-mode' is turned on.")


;;; Options

(defcustom olivetti-body-width
  80
  "Text body width to which to adjust relative margin width.

If an integer, set text body width to that integer in columns; if
a floating point between 0.0 and 1.0, set text body width to
that fraction of the total window width.

An integer is best if you want text body width to remain
constant, while a floating point is best if you want text body
width to change with window width.

The floating point can anything between 0.0 and 1.0 (exclusive),
but it's better to use a value between about 0.33 and 0.9 for
best effect.

This option does not affect file contents."
  :type '(choice (integer 80) (float 0.5))
  :group 'olivetti)
(make-variable-buffer-local 'olivetti-body-width)

(defcustom olivetti-minimum-body-width
  40
  "Minimum width in columns that text body width may be set."
  :type 'integer
  :group 'olivetti)

(defcustom olivetti-hide-mode-line
  nil
  "Hide the mode line."
  :type 'boolean
  :group 'olivetti)

(defcustom olivetti-lighter
  " Olv"
  "Mode-line indicator for `olivetti-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'olivetti)

(defcustom olivetti-recall-visual-line-mode-entry-state
  t
  "Recall the state of `visual-line-mode' upon exiting.

When non-nil, if `visual-line-mode' is inactive upon activating
`olivetti-mode', then `visual-line-mode' will be deactivated upon
exiting. The reverse is not true."
  :type 'boolean
  :group 'olivetti)


;;; Set Environment

(defun olivetti-set-environment (&optional frame)
  "Set text body width to `olivetti-body-width' with relative margins.

Cycle through all windows displaying current buffer and first
find the `olivetti-safe-width' to which to set
`olivetti-body-width', then find the appropriate margin size
relative to each window. Finally set the window margins, taking
care that the maximum size is 0."
  (dolist (window (get-buffer-window-list nil nil t))
    (let* ((n (olivetti-safe-width (if (integerp olivetti-body-width)
                                       (olivetti-scale-width olivetti-body-width)
                                     olivetti-body-width)
                                   window))
           (fringes (window-fringes window))
           (window-width (- (window-total-width window)
                            (+ (/ (car fringes)
                                  (float (frame-char-width)))
                               (/ (cadr fringes)
                                  (float (frame-char-width))))))
           (width (cond ((integerp n) n)
                        ((floatp n) (* window-width
                                       n))))
           (margin (max (round (/ (- window-width
                                     width)
                                  2))
                        0)))
      (set-window-parameter window 'split-window 'olivetti-split-window)
      (set-window-margins window margin margin))
    (if olivetti-hide-mode-line (olivetti-set-mode-line))))

(defun olivetti-reset-all-windows ()
  "Remove Olivetti's parameters and margins from all windows.

Cycle through all windows displaying current buffer and call
`olivetti-reset-window'."
  (dolist (window (get-buffer-window-list nil nil t))
    (olivetti-reset-window window)))

(defun olivetti-reset-window (window)
  "Remove Olivetti's parameters and margins from WINDOW."
  (if (eq (window-parameter window 'split-window) 'olivetti-split-window)
      (set-window-parameter window 'split-window nil))
  (set-window-margins window nil))

(defun olivetti-split-window (&optional window size side pixelwise)
  "Call `split-window' after resetting WINDOW."
  (olivetti-reset-window window)
  (split-window window size side pixelwise))

(defun olivetti-split-window-sensibly (&optional window)
  "Like `olivetti-split-window' but calls `split-window-sensibly'."
  (olivetti-reset-window window)
  (split-window-sensibly window))


;;; Set Mode-Line

(defun olivetti-set-mode-line (&optional arg)
  "Set the mode line formating appropriately.

If ARG is 'toggle, toggle the value of `olivetti-hide-mode-line',
then rerun.

If ARG is 'exit, kill `mode-line-format' then rerun.

If ARG is nil and `olivetti-hide-mode-line' is non-nil, hide the
mode line."
  (cond ((eq arg 'toggle)
         (setq olivetti-hide-mode-line
               (not olivetti-hide-mode-line))
         (olivetti-set-mode-line))
        ((or (eq arg 'exit)
             (not olivetti-hide-mode-line))
         (kill-local-variable 'mode-line-format))
        (olivetti-hide-mode-line
         (setq-local mode-line-format nil))))

(defun olivetti-toggle-hide-mode-line ()
  "Toggle the visibility of the mode-line.

Toggles the value of `olivetti-hide-mode-line' and runs
`olivetti-set-mode-line'."
  (interactive)
  (olivetti-set-mode-line 'toggle))


;;; Calculate Width

(defun olivetti-scale-width (n)
  "Scale N in accordance with the face height.

For compatibility with `text-scale-mode', if
`face-remapping-alist' includes a :height property on the default
face, scale N by that factor, otherwise scale by 1."
  (let ((face-height (or (plist-get (cadr (assq 'default
                                                face-remapping-alist))
                                    :height)
                         1)))
    (round (* n face-height))))

(defun olivetti-safe-width (n window)
  "Parse N to a safe value for `olivetti-body-width' for WINDOW."
  (let ((window-width (- (window-total-width window)
                         (% (window-total-width window) 2)))
        (min-width (+ olivetti-minimum-body-width
                      (% olivetti-minimum-body-width 2))))
    (cond ((integerp n)
           (max (min n window-width) min-width))
          ((floatp n)
           (let ((min-width
                  (string-to-number (format "%0.2f"
                                            (/ (float min-width)
                                               window-width))))
                 (width
                  (string-to-number (format "%0.2f"
                                            (min n 1.0)))))
             (max width min-width)))
          ((user-error "`olivetti-body-width' must be an integer or a float")
           (setq olivetti-body-width
                 (eval (car (get 'olivetti-body-width 'standard-value))))))))


;;; Width Interaction

(defun olivetti-set-width (n)
  "Set text body width to N with relative margins.

N may be an integer specifying columns or a float specifying a
fraction of the window width."
  (interactive
   (list (or current-prefix-arg
             (read-number "Set text body width (integer or float): "
                          olivetti-body-width))))
  (setq olivetti-body-width n)
  (olivetti-set-environment)
  (message "Text body width set to %s" olivetti-body-width))

(defun olivetti-expand (&optional arg)
  "Incrementally increase the value of `olivetti-body-width'.

If prefixed with ARG, incrementally decrease."
  (interactive "P")
  (let* ((p (if arg -1 1))
         (n (cond ((integerp olivetti-body-width)
                   (+ olivetti-body-width (* 2 p)))
                  ((floatp olivetti-body-width)
                   (+ olivetti-body-width (* 0.01 p))))))
    (setq olivetti-body-width (olivetti-safe-width n (selected-window))))
  (olivetti-set-environment)
  (message "Text body width set to %s" olivetti-body-width)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "]" 'olivetti-expand)
     (define-key map "[" 'olivetti-shrink) map)))

(defun olivetti-shrink (&optional arg)
  "Incrementally decrease the value of `olivetti-body-width'.

If prefixed with ARG, incrementally increase."
  (interactive "P")
  (let ((p (unless arg t)))
    (olivetti-expand p)))


;;; Mode Definition

;;;###autoload
(defun turn-on-olivetti-mode ()
  "Turn on `olivetti-mode' unconditionally."
  (interactive)
  (olivetti-mode 1))

(defvar olivetti-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c [") #'olivetti-shrink)
    (define-key map (kbd "C-c ]") #'olivetti-expand)
    (define-key map (kbd "C-c \\") #'olivetti-set-width)
    map)
  "Mode map for `olivetti-mode'.")

;;;###autoload
(define-minor-mode olivetti-mode
  "Olivetti provides a nice writing environment.

Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

When `olivetti-hide-mode-line' is non-nil, the mode line is also
hidden."
  :init-value nil
  :lighter olivetti-lighter
  (if olivetti-mode
      (progn
        (dolist (hook '(window-configuration-change-hook
                        window-size-change-functions
                        after-setting-font-hook
                        text-scale-mode-hook))
          (add-hook hook 'olivetti-set-environment t t))
        (add-hook 'change-major-mode-hook
                  'olivetti-reset-all-windows nil t)
        (setq-local split-window-preferred-function
                    'olivetti-split-window-sensibly)
        (setq olivetti--visual-line-mode visual-line-mode)
        (unless olivetti--visual-line-mode (visual-line-mode 1))
        (olivetti-set-environment))
    (dolist (hook '(window-configuration-change-hook
                    window-size-change-functions
                    after-setting-font-hook
                    text-scale-mode-hook))
      (remove-hook hook 'olivetti-set-environment t))
    (olivetti-reset-all-windows)
    (olivetti-set-mode-line 'exit)
    (if (and olivetti-recall-visual-line-mode-entry-state
             (not olivetti--visual-line-mode))
        (visual-line-mode 0))
    (kill-local-variable 'split-window-preferred-function)
    (kill-local-variable 'olivetti--visual-line-mode)))



(provide 'olivetti)

;;; olivetti.el ends here
