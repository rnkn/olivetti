;;; olivetti.el --- Minor mode for a nice writing environment

;; Copyright (c) 2014-2016 Paul Rankin

;; Author: Paul Rankin <hello@paulwrankin.com>
;; Keywords: wp

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

;; Olivetti
;; ========

;; Olivetti is a simple Emacs minor mode for a nice writing environment.

;; Features
;; --------

;; - Set a desired text body width to automatically resize window margins
;;   to keep the text comfortably in the middle of the window.
;; - Text body width can be the number of characters (an integer) or a
;;   fraction of the window width (a float between 0.0 and 1.0).
;; - Interactively change body width with:
;;   `olivetti-shrink` C-c [
;;   `olivetti-expand` C-c ]
;;   and `olivetti-set-width`.
;; - If `olivetti-body-width` is an integer, the text body width will scale
;;   with use of `text-scale-mode`, whereas if a fraction (float) then the
;;   text body width will remain at that fraction.
;; - Optionally remember the state of `visual-line-mode` on entry and
;;   recall its state on exit.
;; - Optionally hide the mode-line for distraction-free writing.

;; Requirements
;; ------------

;; - Emacs 24.4

;; Installation
;; ------------

;; Olivetti is available through [MELPA][] and [MELPA-stable][]. I
;; encourage installing the stable version.

;; Alternately, download the [latest release][] and put it in your
;; `load-path`.

;; [melpa]: https://melpa.org/ "MELPA"
;; [melpa-stable]: https://stable.melpa.org/ "MELPA"
;; [latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

;; Known Bugs
;; ----------

;; - `linum-mode` currently has a bug that overwrites margin settings,
;;   making it incompatible with Olivetti. More information here:
;;   <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20674>

;; Please report bugs on GitHub [Issues][] page.

;; [issues]: https://github.com/rnkn/olivetti/issues "Olivetti issues"

;; History
;; -------

;; See [Releases][].

;; [releases]: https://github.com/rnkn/olivetti/releases "Olivetti releases"

;;; Code:

(defgroup olivetti ()
  "Minor mode for a nice writing environment"
  :prefix "olivetti-"
  :group 'wp)

;;; Customizable Variables =====================================================

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

(defcustom olivetti-recall-visual-line-mode-entry-state
  t
  "Recall the state of `visual-line-mode' upon exiting.

When non-nil, if `visual-line-mode' is inactive upon activating
`olivetti-mode', then `visual-line-mode' will be deactivated upon
exiting. The reverse is not true."
  :type 'boolean
  :group 'olivetti)

;;; Variables ==================================================================

(defvar-local olivetti--visual-line-mode
  nil
  "Non-nil if `visual-line-mode' is active when `olivetti-mode' is turned on.")

;;; Functions ==================================================================

(defun olivetti-set-mode-line (&optional arg)
  "Set the mode line formating appropriately.

If ARG is 'toggle, toggle the value of `olivetti-hide-mode-line',
then rerun.

If ARG is 'exit, kill `mode-line-format' then rerun.

If ARG is nil and `olivetti-hide-mode-line' is non-nil, hide the
mode line."
  (cond ((equal arg 'toggle)
         (setq olivetti-hide-mode-line
               (null olivetti-hide-mode-line))
         (olivetti-set-mode-line))
        ((or (equal arg 'exit)
             (null olivetti-hide-mode-line))
         (kill-local-variable 'mode-line-format))
        (olivetti-hide-mode-line
         (setq-local mode-line-format nil))))
  ;; (redraw-frame (selected-frame)))

(defun olivetti-scale-width (n)
  "Scale N in accordance with the face height.

For compatibility with `text-scale-mode', if
`face-remapping-alist' includes a :height property on the default
face, scale N by that factor, otherwise scale by 1."
  (let ((face-height (or (plist-get (cadr (assoc 'default
                                                 face-remapping-alist))
                                    :height)
                         1)))
    (round (* n face-height))))

(defun olivetti-safe-width (n)
  "Parse N to a safe value for `olivetti-body-width'."
  (let ((window-width (- (window-total-width)
                         (% (window-total-width) 2)))
        (min-width (+ olivetti-minimum-body-width
                      (% olivetti-minimum-body-width 2))))
    (cond ((integerp n)
           (let ((width (min n window-width)))
             (max width min-width)))
          ((floatp n)
           (let ((min-width
                  (string-to-number (format "%0.2f"
                                            (/ (float min-width)
                                               window-width))))
                 (width
                  (string-to-number (format "%0.2f"
                                            (min n 1.0)))))
             (max width min-width)))
          ((message "`olivetti-body-width' must be an integer or a float")
           (setq olivetti-body-width
                 (car (get 'olivetti-body-width 'standard-value)))))))

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

(defun olivetti-set-environment (&optional arg)
  "Set text body width to `olivetti-body-width' with relative margins.

Cycle through all windows displaying current buffer and:

If ARG is 'exit then cycle through all windows displaying the
current buffer and set window margins to nil.

If ARG is nil, first find the `olivetti-safe-width' to which to
set `olivetti-body-width', then find the appropriate margin size
relative to each window. Finally set the window margins, taking
care that the maximum size is 0."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (if (equal arg 'exit)
        (set-window-margins window nil nil)
      (let* ((n (olivetti-safe-width (if (integerp olivetti-body-width)
                                         (olivetti-scale-width olivetti-body-width)
                                       olivetti-body-width)))
             (width (cond ((integerp n) n)
                          ((floatp n) (* (window-total-width window)
                                         n))))
             (margin (max (round (/ (- (window-total-width window) width)
                                    2))
                          0)))
        (set-window-margins window margin margin)))))

(defun olivetti-toggle-hide-mode-line ()
  "Toggle the visibility of the mode-line.

Toggles the value of `olivetti-hide-mode-line' and runs
`olivetti-set-mode-line'."
  (interactive)
  (olivetti-set-mode-line 'toggle))

(defun olivetti-expand (&optional arg)
  "Incrementally increase the value of `olivetti-body-width'.

If prefixed with ARG, incrementally decrease."
  (interactive "P")
  (let* ((p (if arg -1 1))
         (n (cond ((integerp olivetti-body-width)
                   (+ olivetti-body-width (* 2 p)))
                  ((floatp olivetti-body-width)
                   (+ olivetti-body-width (* 0.01 p))))))
    (setq olivetti-body-width (olivetti-safe-width n)))
  (olivetti-set-environment)
  (message "Text body width set to %s" olivetti-body-width))

(defun olivetti-shrink (&optional arg)
  "incrementally decrease the value of `olivetti-body-width'.

If prefixed with ARG, incrementally increase."
  (interactive "P")
  (let ((p (unless arg t)))
    (olivetti-expand p)))

;;; Mode Definition ============================================================

;;;###autoload
(defun turn-on-olivetti-mode ()
  "Turn on `olivetti-mode' unconditionally."
  (interactive)
  (olivetti-mode 1))

;;;###autoload
(define-minor-mode olivetti-mode
  "Olivetti provides a nice writing environment.

Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

When `olivetti-hide-mode-line' is non-nil, the mode line is also
hidden."
  :init-value nil
  :lighter " Olv"
  :keymap '(([?\C-c ?\[] . olivetti-shrink)
            ([?\C-c ?\]] . olivetti-expand))
  (if olivetti-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  'olivetti-set-environment nil t)
        (add-hook 'after-setting-font-hook
                  'olivetti-set-environment nil t)
        (add-hook 'text-scale-mode-hook
                  'olivetti-set-environment nil t)
        (setq olivetti--visual-line-mode visual-line-mode)
        (unless olivetti--visual-line-mode
          (visual-line-mode 1))
        (if olivetti-hide-mode-line
            (olivetti-set-mode-line))
        (olivetti-set-environment))
    (remove-hook 'window-configuration-change-hook
                 'olivetti-set-environment t)
    (remove-hook 'after-setting-font-hook
                 'olivetti-set-environment t)
    (remove-hook 'text-scale-mode-hook
                 'olivetti-set-environment t)
    (olivetti-set-mode-line 'exit)
    (olivetti-set-environment 'exit)
    (if (and olivetti-recall-visual-line-mode-entry-state
             (null olivetti--visual-line-mode))
        (visual-line-mode 0))
    (kill-local-variable 'olivetti--visual-line-mode)))

(provide 'olivetti)
;;; olivetti.el ends here
