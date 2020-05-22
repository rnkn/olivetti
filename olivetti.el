;;; olivetti.el --- Minor mode for a nice writing environment -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2019  Paul Wiliam Rankin
;; Copyright (c) 2019       Free Software Foundation, Inc.
;; Copyright (c) 2019-2020  Paul Wiliam Rankin

;; Author: William Rankin <code@william.bydasein.com>
;; Keywords: wp, text
;; Version: 1.10.0-beta
;; Package-Requires: ((emacs "24.5"))
;; URL: https://gthub.com/rnkn/olivetti

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; # Olivetti #

;; A simple Emacs minor mode for a nice writing environment.

;; ## Features ##

;; - Set a desired text body width to automatically resize window margins to
;;   keep the text comfortably in the middle of the window.
;; - Text body width can be the number of characters (an integer) or a fraction
;;   of the window width (a float between 0.0 and 1.0).
;; - Interactively change body width with:
;;   olivetti-shrink C-c { { { ...
;;   olivetti-expand C-c } } } ...
;;   olivetti-set-width C-c \
;; - If olivetti-body-width is an integer, the text body width will scale with
;;   use of text-scale-mode, whereas if a fraction (float) then the text body
;;   width will remain at that fraction.
;; - Optionally remember the state of visual-line-mode on entry and recall its
;;   state on exit.

;; Olivetti keeps everything it does buffer-local, so you can write prose in one
;; buffer and code in another, side-by-side in the same frame. For those looking
;; for a hardcore distraction-free writing mode with a much larger scope, I
;; recommend writeroom-mode: https://github.com/joostkremers/writeroom-mode.

;; ## Requirements ##

;; - Emacs 25.3

;; ## Installation ##

;; The latest stable release of Olivetti is available via [MELPA-stable]
;; and can be installed with:

;;     M-x package-install RET olivetti RET

;; Alternately, download the [latest release], move this file into your
;; load-path and add to your .emacs/init.el file:

;;     (require 'olivetti)

;; If you prefer the latest but perhaps unstable version, install via
;; [MELPA], or clone the repository into your load-path and require as
;; above:

;;     git clone https://github.com/rnkn/olivetti.git

;; [melpa]: https://melpa.org/#/olivetti "MELPA"
;; [melpa-stable]: https://stable.melpa.org/#/olivetti "MELPA-stable"
;; [latest release]: https://github.com/rnkn/olivetti/releases/latest "Olivetti latest release"

;; ## Contributing ##

;; Please report bugs and request features at:
;; https://github.com/rnkn/olivetti/issues

;; ## Hints ##

;; To always use a different width for a specific file, set a File
;; Variable:

;;     M-x add-file-local-variable RET olivetti-body-width RET 66 RET

;; See (info "(emacs) File Variables")


;;; Code:

(defgroup olivetti ()
  "Minor mode for a nice writing environment"
  :prefix "olivetti-"
  :group 'text)


;;; Variables

(eval-when-compile
  (require 'lisp-mnt)
  (defconst olivetti-version
    (lm-version load-file-name)))

(defvar-local olivetti--visual-line-mode
  nil
  "Non-nil if `visual-line-mode' is active when `olivetti-mode' is turned on.")


;;; Options

(defcustom olivetti-mode-hook
  nil
  "Hook for `olivetti-mode', run after the mode is activated."
  :type 'hook
  :safe 'hook)

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

(defcustom olivetti-enable-visual-line-mode
  t
  "When non-nil, `visual-line-mode' is enabled with `olivetti-mode'."
  :type 'boolean
  :safe 'booleanp)

(defcustom olivetti-recall-visual-line-mode-entry-state
  t
  "Recall the state of `visual-line-mode' upon exiting.

When non-nil, remember if `visual-line-mode' was enabled or not
upon activating `olivetti-mode' and restore that state upon
exiting."
  :type 'boolean
  :safe 'booleanp)


;;; Set Windows

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
          (t
           (message "`olivetti-body-width' must be an integer or a float")
           (eval (car (get 'olivetti-body-width 'standard-value)))))))

(defun olivetti-scale-width (n)
  "Scale N in accordance with the face height.

For compatibility with `text-scale-mode', if
`face-remapping-alist' includes a :height property on the default
face, scale N by that factor if it is a fraction, by (height/100)
if it is an integer, and otherwise scale by 1 (i.e. return N)."
  (let
      ((height (plist-get (cadr (assq 'default face-remapping-alist)) :height)))
    (cond
     ((integerp height) (* n (/ height 100.0)))
     ((floatp height) (* n height))
     (t (* n 1)))))

(defun olivetti-reset-window (window)
  "Remove Olivetti's parameters and margins from WINDOW."
  (when (eq (window-parameter window 'split-window) 'olivetti-split-window)
    (set-window-parameter window 'split-window nil))
  (set-window-margins window nil)
  (set-window-parameter window 'min-margins nil))

(defun olivetti-reset-all-windows ()
  "Call `olivetti-reset-windows' on all windows in current frame."
  (mapc #'olivetti-reset-window (window-list nil 'no-minibuf)))

(defun olivetti-split-window (&optional window size side pixelwise)
  "Call `split-window' after resetting WINDOW.
Pass SIZE, SIDE and PIXELWISE unchanged."
  (olivetti-reset-all-windows)
  (split-window window size side pixelwise))

(defun olivetti-split-window-sensibly (&optional window)
  "Like `olivetti-split-window' but call `split-window-sensibly'.
Pass WINDOW unchanged."
  (olivetti-reset-all-windows)
  (split-window-sensibly window))

(defun olivetti-set-window (window-or-frame)
  "Balance window margins displaying current buffer.

If WINDOW-OR-FRAME is a frame, cycle through windows displaying
current buffer in that frame, otherwise only work on the selected
window.

First find the `olivetti-safe-width' to which to set
`olivetti-body-width', then find the appropriate margin size
relative to each window. Finally set the window margins, taking
care that the maximum size is 0."
  (if (framep window-or-frame)
      (mapc #'olivetti-set-window (get-buffer-window-list nil nil window-or-frame))
    ;; WINDOW-OR-FRAME passed below *must* be a window
    (with-selected-window window-or-frame
      (when olivetti-mode
        ;; (olivetti-reset-window window-or-frame)
        (let ((width (olivetti-safe-width olivetti-body-width window-or-frame))
              (frame (window-frame window-or-frame))
              (window-width (window-total-width window-or-frame))
              (fringes (window-fringes window-or-frame))
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
          (set-window-margins window-or-frame left-margin right-margin))
        (set-window-parameter window-or-frame 'split-window 'olivetti-split-window)
        (set-window-parameter window-or-frame 'min-margins (cons 0 0))))))

(defun olivetti-set-buffer-windows ()
  "Balance window margins in all windows displaying current buffer.

Cycle through all windows in all visible frames displaying the
current buffer, and call `olivetti-set-window'."
  (mapc #'olivetti-set-window (get-buffer-window-list nil nil 'visible)))


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
  (olivetti-set-buffer-windows)
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
  (olivetti-set-buffer-windows)
  (message "Text body width set to %s" olivetti-body-width)
  (unless overriding-terminal-local-map
    (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
          (map (cdr olivetti-mode-map)))
      (when (< 0 (length prefix-keys))
        (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
        (when (consp map) (set-transient-map (cdr map) t))))))

(defun olivetti-shrink (&optional arg)
  "Incrementally decrease the value of `olivetti-body-width'.

If prefixed with ARG, incrementally increase."
  (interactive "P")
  (let ((p (unless arg t)))
    (olivetti-expand p)))


;;; Keymap

(defvar olivetti-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c }") #'olivetti-expand)
    (define-key map (kbd "C-c {") #'olivetti-shrink)
    (define-key map (kbd "C-c \\") #'olivetti-set-width)
    map)
  "Mode map for `olivetti-mode'.")


;;; Mode Definition

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
        (cond ((<= emacs-major-version 24)
               (add-hook 'window-configuration-change-hook
                         #'olivetti-set-buffer-windows t t))
              ((<= emacs-major-version 26)
               (add-hook 'window-configuration-change-hook
                         #'olivetti-set-buffer-windows t t)
               (add-hook 'window-size-change-functions
                         #'olivetti-set-window t t))
              ((<= 27 emacs-major-version)
               (add-hook 'window-size-change-functions
                         #'olivetti-set-window t t)))
        (add-hook 'change-major-mode-hook
                  #'olivetti-reset-all-windows nil t)
        (add-hook 'text-scale-mode-hook
                  #'olivetti-set-buffer-windows t t)
        (setq-local split-window-preferred-function
                    #'olivetti-split-window-sensibly)
        (setq olivetti--visual-line-mode visual-line-mode)
        (when (and olivetti-enable-visual-line-mode
                   (not olivetti--visual-line-mode))
          (visual-line-mode 1))
        (olivetti-set-buffer-windows))
    (remove-hook 'window-configuration-change-hook
                 #'olivetti-set-buffer-windows t)
    (remove-hook 'window-size-change-functions
                 #'olivetti-set-window t)
    (remove-hook 'text-scale-mode-hook
                 #'olivetti-set-window t)
    (olivetti-reset-all-windows)
    (when olivetti-recall-visual-line-mode-entry-state
      (visual-line-mode (if olivetti--visual-line-mode 1 0)))
    (kill-local-variable 'split-window-preferred-function)
    (kill-local-variable 'olivetti--visual-line-mode)))



(provide 'olivetti)

;;; olivetti.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; End:
