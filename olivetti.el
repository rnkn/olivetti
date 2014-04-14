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
  :group 'wp
  :group 'fountain
  :group 'markdown)

;;; customizable variables =============================================

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

(defcustom olivetti-hide-menu-bar nil
  "Turn off `menu-bar-mode'."
  :type 'boolean
  :group 'olivetti)

(defcustom olivetti-hide-tool-bar t
  "Turn off `tool-bar-mode'."
  :type 'boolean
  :group 'olivetti)

(defcustom olivetti-hide-mode-line nil
  "Hide the mode line.
Default is nil because this can cause display issues in console
mode."
  :type 'boolean
  :group 'olivetti)

(defcustom olivetti-hide-fringes t
  "Hide fringes."
  :type 'boolean
  :group 'olivetti)

(defcustom olivetti-delete-selection t
  "Turn on `delete-selection-mode'."
  :type 'boolean
  :group 'olivetti)

;;; functions ==========================================================

(defun olivetti-set-mode-line (&optional arg)
  (interactive)
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
    (set-window-margins (selected-window) margin margin))
  (when olivetti-hide-fringes
    (set-window-fringes (selected-window) 0 0 t)))

;;; menu ===============================================================

(defvar olivetti-mode-map
  (make-sparse-keymap)
  "Mode map for `olivetti-mode'.")

(easy-menu-define olivetti-mode-menu olivetti-mode-map
  "Menu for Olivetti Mode."
  '("Olivetti"
    ["Hide Menu Bar" ignore
     :style toggle
     :selected olivetti-hide-menu-bar]
    ["Hide Tool Bar" ignore
     :style toggle
     :selected olivetti-hide-tool-bar]
    ["Hide Mode Line" (olivetti-set-mode-line 'toggle)
     :style toggle
     :selected olivetti-hide-mode-line]))

;; mode definition =====================================================

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
        (when olivetti-hide-menu-bar
          (menu-bar-mode 0))
        (when olivetti-hide-tool-bar
          (tool-bar-mode 0))
        (when olivetti-delete-selection
          (delete-selection-mode 1))
        (add-hook 'window-configuration-change-hook
                  'olivetti-set-environment nil t)
        (run-hooks 'window-configuration-change-hook))
    (olivetti-set-mode-line 'exit)
    (remove-hook 'window-configuration-change-hook
                 'olivetti-set-environment t)
    (set-window-margins nil nil)))

(provide 'olivetti)
;;; olivetti.el ends here
