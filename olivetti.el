;;; olivetti.el --- Minor mode for a nice writing environment -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2019  Free Software Foundation, Inc.

;; Author: Paul Rankin <hello@paulwrankin.com>
;; Keywords: wp, text
;; Version: 1.7.0
;; Package-Requires: ((emacs "24.5"))

;; This file is part of GNU Emacs.

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
;;   `olivetti-shrink' C-c [ [ [ ...
;;   `olivetti-expand' C-c ] ] ] ...
;;   `olivetti-set-width' C-c \
;; - If `olivetti-body-width' is an integer, the text body width will scale
;;   with use of `text-scale-mode`, whereas if a fraction (float) then the
;;   text body width will remain at that fraction.
;; - Optionally remember the state of `visual-line-mode' on entry and
;;   recall its state on exit.

;; Olivetti keeps everything it does buffer-local, so you can write prose in one
;; buffer and code in another, side-by-side in the same frame. For those looking
;; for a hardcore distraction-free writing mode with a much larger scope, I
;; recommend writeroom-mode: <https://github.com/joostkremers/writeroom-mode>.

;; Requirements
;; ------------

;; - Emacs 24.5

;; Installation
;; ------------

;; Olivetti is now part of GNU ELPA and can be installed with M-x package-install
;; RET olivetti RET.

;; Bugs
;; ----

;; - Emacs 27.x has a compatibility change in window.c that will cause errors in
;;   redisplay by passing a window instead of a frame as argument.

;; To report bugs, please use `M-x report-emacs-bug RET` or send an email to
;; <bug-gnu-emacs@gnu.org>

;; Hints
;; -----

;; To always use a different width for a specific file, set a File Variable
;; specifying `olivetti-body-width':

;; M-x add-file-local-variable RET olivetti-body-width RET 66 RET

;; See (info "(emacs) File Variables").


;;; Code:

(defgroup olivetti ()
  "Minor mode for a nice writing environment"
  :prefix "olivetti-"
  :group 'text)


;;; Variables

(defvar-local olivetti--visual-line-mode
  nil
  "Non-nil if `visual-line-mode' is active when `olivetti-mode' is turned on.")


;;; Options

(defcustom olivetti-body-width
  70
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
  :type '(choice (integer 70) (float 0.5))
  :safe 'numberp)
(make-variable-buffer-local 'olivetti-body-width)

(defcustom olivetti-minimum-body-width
  40
  "Minimum width in columns that text body width may be set."
  :type 'integer
  :safe 'integerp)

(defcustom olivetti-lighter
  " Olv"
  "Mode-line indicator for `olivetti-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp)

(defcustom olivetti-recall-visual-line-mode-entry-state
  t
  "Recall the state of `visual-line-mode' upon exiting.

When non-nil, if `visual-line-mode' is inactive upon activating
`olivetti-mode', then `visual-line-mode' will be deactivated upon
exiting. The reverse is not true."
  :type 'boolean
  :safe 'booleanp)


;;; Set Environment

(defun olivetti-set-all-margins ()
  "Balance window margins in all windows displaying current buffer.

Cycle through all windows in all frames displaying the current
buffer, and call `olivetti-set-margins'."
  (dolist (window (get-buffer-window-list nil nil t))
    (olivetti-set-margins window)))

(defun olivetti-set-margins (&optional frame-or-window)
  "Balance window margins displaying current buffer.

If FRAME-OR-WINDOW is a frame, cycle through windows displaying
current buffer in that frame, otherwise only work on the selected
window.

First find the `olivetti-safe-width' to which to set
`olivetti-body-width', then find the appropriate margin size
relative to each window. Finally set the window margins, taking
care that the maximum size is 0."
  (if (framep frame-or-window)
      (dolist (window (get-buffer-window-list nil nil frame-or-window))
        (olivetti-set-margins window))
    ;; FRAME-OR-WINDOW passed below *must* be a window
    (olivetti-reset-window frame-or-window)
    (let ((width (olivetti-safe-width olivetti-body-width frame-or-window))
          (frame (window-frame frame-or-window))
          (window-width (window-total-width frame-or-window))
          (fringes (window-fringes frame-or-window))
          left-fringe right-fringe margin-total left-margin right-margin)
      (cond ((integerp width)
             (setq width (olivetti-scale-width width)))
            ((floatp width)
             (setq width (* window-width width))))
      (setq left-fringe (/ (car fringes) (float (frame-char-width frame)))
            right-fringe (/ (cadr fringes) (float (frame-char-width frame))))
      (setq margin-total (max (/ (- window-width width) 2) 0)
            left-margin (max (round (- margin-total left-fringe)) 0)
            right-margin (max (round (- margin-total right-fringe)) 0))
      (set-window-parameter frame-or-window 'split-window 'olivetti-split-window)
      (set-window-margins frame-or-window left-margin right-margin))))

(defun olivetti-reset-all-windows ()
  "Remove Olivetti's parameters and margins from all windows.

Cycle through all windows displaying current buffer and call
`olivetti-reset-window'."
  (dolist (window (get-buffer-window-list nil nil t))
    (olivetti-reset-window window)))

(defun olivetti-reset-window (window)
  "Remove Olivetti's parameters and margins from WINDOW."
  (when (eq (window-parameter window 'split-window) 'olivetti-split-window)
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


;;; Calculate Width

(defun olivetti-scale-width (n)
  "Scale N in accordance with the face height.

For compatibility with `text-scale-mode', if
`face-remapping-alist' includes a :height property on the default
face, scale N by that factor if it is a fraction, by (height/100)
if it is an integer, and otherwise scale by 1."
  (let
      ((height (plist-get (cadr (assq 'default face-remapping-alist)) :height)))
    (cond
     ((integerp height) (* n (/ height 100.0)))
     ((floatp height) (* n height))
     (t n))))

(defun olivetti-safe-width (width window)
  "Parse WIDTH to a safe value for `olivetti-body-width' for WINDOW.

May return a float with many digits of precision."
  (let ((window-width (window-total-width window))
        (fringes (window-fringes window))
        (min-width (+ olivetti-minimum-body-width
                      (% olivetti-minimum-body-width 2))))
    (setq window-width
          (- window-width
             (/ (* (max (car fringes) (cadr fringes)) 2)
                (float (frame-char-width (window-frame window))))
             (% window-width 2)))
    (cond ((integerp width)
           (max min-width (min width (floor window-width))))
          ((floatp width)
           (max (/ min-width window-width) (min width 1.0)))
          ((user-error "`olivetti-body-width' must be an integer or a float")
           ;; FIXME: This code is unreachable since we signal an error before
           ;; getting here!?
           (eval (car (get 'olivetti-body-width 'standard-value)) t)))))


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
  (olivetti-set-margins)
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
  (olivetti-set-margins)
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

(defvar olivetti-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c [") #'olivetti-shrink)
    (define-key map (kbd "C-c ]") #'olivetti-expand)
    (define-key map (kbd "C-c \\") #'olivetti-set-width)
    map)
  "Mode map for `olivetti-mode'.")

(define-obsolete-function-alias 'turn-on-olivetti-mode
  #'olivetti-mode "1.7.0")

;;;###autoload
(define-minor-mode olivetti-mode
  "Olivetti provides a nice writing environment.

Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'."
  :init-value nil
  :lighter olivetti-lighter
  (if olivetti-mode
      (progn
        (add-hook 'window-size-change-functions
                  #'olivetti-set-margins t t)
        (add-hook 'window-configuration-change-hook
                  'olivetti-set-all-margins t t)
        (add-hook 'change-major-mode-hook
                  #'olivetti-reset-all-windows nil t)
        (setq-local split-window-preferred-function
                    #'olivetti-split-window-sensibly)
        (setq olivetti--visual-line-mode visual-line-mode)
        (unless olivetti--visual-line-mode (visual-line-mode 1))
        (olivetti-set-all-margins))
    (remove-hook 'window-configuration-change-hook
                 #'olivetti-set-all-margins t)
    (remove-hook 'window-size-change-functions
                 #'olivetti-set-margins t)
    (olivetti-reset-all-windows)
    (when (and olivetti-recall-visual-line-mode-entry-state
               (not olivetti--visual-line-mode))
      (visual-line-mode 0))
    (kill-local-variable 'split-window-preferred-function)
    (kill-local-variable 'olivetti--visual-line-mode)))



(provide 'olivetti)

;;; olivetti.el ends here
