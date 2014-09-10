;;; olivetti.el --- Minor mode for a nice writing environment

;; Copyright (C) 2014  Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
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

;;

;;; Code:

(defgroup olivetti ()
  "Minor mode for a nice writing environment"
  :prefix "olivetti-"
  :group 'wp)

;;; Customizable Variables =====================================================

(defcustom olivetti-mode-hook
  '(turn-on-visual-line-mode)
  "Mode hook for `olivetti-mode', run after mode is turned on."
  :type 'hook
  :group 'olivetti)

(defcustom olivetti-body-width 66
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
  :type '(choice (integer 66) (float 0.5))
  :group 'olivetti)

(defcustom olivetti-hide-mode-line nil
  "Hide the mode line.
Can cause display issues in console mode."
  :type 'boolean
  :group 'olivetti)

(defcustom olivetti-delete-selection t
  "Turn on `delete-selection-mode'."
  :type 'boolean
  :group 'olivetti)

;;; Functions ==================================================================

(defun olivetti-set-mode-line (&optional arg)
  "Set the mode line formating appropriately.
If ARG is 'toggle, toggle the value of `olivetti-hide-mode-line',
then rerun. If ARG is 'exit, kill `mode-line-format' then rerun.
If ARG is nil and `olivetti-hide-mode-line' is non-nil, hide the
mode line. Finally redraw the frame."
  (cond ((equal arg 'toggle)
         (setq olivetti-hide-mode-line
               (null olivetti-hide-mode-line))
         (olivetti-set-mode-line))
        ((or (equal arg 'exit)
             (null olivetti-hide-mode-line))
         (kill-local-variable 'mode-line-format))
        (olivetti-hide-mode-line
         (setq-local mode-line-format nil)))
  (redraw-frame (selected-frame)))

(defun olivetti-set-environment ()
  "Set text body width to `olivetti-body-width' with relative margins."
  (let* ((n olivetti-body-width)
         (width
          (cond ((integerp n) n)
                ((and (floatp n)
                      (< n 1)
                      (> n 0))
                 (* (window-total-width) n))
                ((error "`olivetti-body-width' must be an integer or a floating point between 0.0 and 1.0"))))
         (margin
          (round (/ (- (window-total-width) width) 2))))
    (set-window-margins (selected-window) margin margin)))

(defun olivetti-toggle-hide-modeline ()
  "Toggle the visibility of the modeline.
Toggles the value of `olivetti-hide-mode-line' and runs
`olivetti-set-mode-line'."
  (interactive)
  (olivetti-set-mode-line 'toggle))

;; Mode Definition =============================================================

;;;###autoload
(defun turn-on-olivetti-mode ()
  "Turn on `olivetti-mode' unconditionally."
  (interactive)
  (olivetti-mode 1))

;;;###autoload
(define-minor-mode olivetti-mode
  "Olivetti provides a nice writing environment.

Window margins are set to relative widths to accomodate a text
body width set in `olivetti-body-width'.

When `olivetti-hide-mode-line' is non-nil, the mode line is also
hidden."
  :init-value nil
  :lighter " Olv"
  (if olivetti-mode
      (progn
        (olivetti-set-mode-line)
        (setq-local scroll-conservatively 101)
        (when olivetti-delete-selection
          (delete-selection-mode 1))
        (add-hook 'window-configuration-change-hook
                  'olivetti-set-environment nil t)
        (run-hooks 'window-configuration-change-hook))
    (olivetti-set-mode-line 'exit)
    (set-window-margins nil nil)
    (remove-hook 'window-configuration-change-hook
                 'olivetti-set-environment t)))

(provide 'olivetti)
;;; olivetti.el ends here
