;;; olivetti.el --- Minor mode for a nice writing environment -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2023  Paul W. Rankin

;; Author: Paul W. Rankin <hello@paulwrankin.com>
;; Keywords: wp, text
;; Version: 2.0.5
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Olivetti
;; ========

;; A simple Emacs minor mode for a nice writing environment.

;; Features
;; --------

;;  - Set a desired text body width to automatically resize window margins
;;    to keep the text comfortably in the middle of the window.
;;  - Text body width can be the number of characters (an integer), a fraction of
;;    the window width (a float between 0.0 and 1.0), or nil which uses the value
;;    of fill-column +2.
;;  - Interactively change body width with:
;;    olivetti-shrink C-c { { { ...
;;    olivetti-expand C-c } } } ...
;;    olivetti-set-width C-c |
;;  - If olivetti-body-width is an integer, the text body width will
;;    scale with use of text-scale-mode, whereas if a fraction (float) then
;;    the text body width will remain at that fraction.
;;  - Change the way the text body margins look with option olivetti-style: use
;;    margins, fringes, or both for a fancy "page" look.
;;  - Customize olivetti-fringe face to affect only Olivetti buffers.
;;  - Optionally remember the state of visual-line-mode on entry and
;;    recall its state on exit.

;; Olivetti keeps everything it does buffer-local, so you can write prose
;; in one buffer and code in another, side-by-side in the same frame.


;; Requirements
;; ------------

;;  - Emacs 24.4


;; Installation
;; ------------

;; The latest stable release of Olivetti is available via
;; [MELPA-stable][1]. First, add MELPA-stable to your package archives:

;;     M-x customize-option RET package-archives RET

;; Insert an entry named melpa-stable with URL:
;; https://stable.melpa.org/packages/

;; You can then find the latest stable version of olivetti in the
;; list returned by:

;;     M-x list-packages RET

;; If you prefer the latest but perhaps unstable version, do the above
;; using [MELPA][2].


;; Advanced Installation
;; ---------------------

;; Download the latest tagged release, move this file into your load-path
;; and add to your init.el file:

;;     (require 'olivetti)

;; If you wish to contribute to or alter Olivetti's code, clone the
;; repository into your load-path and require as above:

;;     git clone https://github.com/rnkn/olivetti.git


;; Bugs and Feature Requests
;; -------------------------

;; Use GitHub issues or send me an email (address in the package header).
;; For bugs, please ensure you can reproduce with:

;;     $ emacs -Q -l olivetti.el


;; Hints
;; -----

;; To always use a different width for a specific file, set a File
;; Variable:

;;     M-x add-file-local-variable RET olivetti-body-width RET 66 RET

;; See (info "(emacs) File Variables")


;; Alternatives
;; ------------

;; For those looking for a hardcore distraction-free writing mode with a much
;; larger scope, I recommend [Writeroom Mode](https://github.com/joostkremers/writeroom-mode).


;; [1]: https://stable.melpa.org/#/olivetti
;; [2]: https://melpa.org/#/olivetti

;;; Code:

(require 'fringe)

(defgroup olivetti ()
  "Minor mode for a nice writing environment."
  :prefix "olivetti-"
  :group 'text)


;;; Internal Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'lisp-mnt)
  (defconst olivetti-version
    (lm-version load-file-name)))

(defvar-local olivetti--visual-line-mode
  nil
  "Value of `visual-line-mode' when when `olivetti-mode' is enabled.")

(defvar-local olivetti--split-window-preferred-function
  nil
  "Value of `split-window-preferred-function' at initialization.")

(defvar-local olivetti--face-remap
  nil
  "Saved cookie from `face-remap-add-relative' at initialization.")


;;; Options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom olivetti-mode-on-hook
  '(visual-line-mode)
  "Hook for `olivetti-mode', run after the mode is activated."
  :type 'hook
  :options '(visual-line-mode)
  :safe 'hook)

(defcustom olivetti-mode-off-hook
  nil
  "Hook for `olivetti-mode', run after the mode is deactivated."
  :type 'hook
  :safe 'hook)

(defcustom olivetti-body-width
  nil
  "Text body width to which to adjust relative margin width.
If an integer, set text body width to that integer in columns; if
a floating point between 0.0 and 1.0, set text body width to that
fraction of the total window width. If nil (the default), use the
value of `fill-column' + 2.

An integer is best if you want text body width to remain
constant, while a floating point is best if you want text body
width to change with window width.

The floating point can anything between 0.0 and 1.0 (exclusive),
but use a value between about 0.33 and 0.9 for best effect.

This option does not affect file contents."
  :type '(choice (const :tag "Value of fill-column + 2" nil)
                 (integer 72)
                 (float 0.5))
  :safe (lambda (value)
          (or (numberp value) (null value))))
(make-variable-buffer-local 'olivetti-body-width)

(defcustom olivetti-minimum-body-width
  40
  "Minimum width in columns of text body."
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
When non-nil, remember if `visual-line-mode' was enabled or not
upon activating `olivetti-mode' and restore that state upon
exiting."
  :type 'boolean
  :safe 'booleanp)

(defcustom olivetti-style
  nil
  "Window elements used to balance the text body.
Valid options are:

    nil         use margins (default)
    t           use fringes
    fancy       use both margins with fringes outside

n.b. Fringes are only available on a graphical window system and
will fall back to margins on console."
  :type '(choice (const :tag "Margins" nil)
                 (const :tag "Fringes" t)
                 (const :tag "Fringes and Margins" fancy))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (featurep 'olivetti)
           (olivetti-reset-all-windows))))

(defcustom olivetti-margin-width
  10
  "Width in columns of margin between text body and fringes.
Only has any effect when `olivetti-style' is set to `fancy'."
  :type '(choice (const :tag "None" nil)
                 (integer :tag "Columns" 10))
  :safe 'integerp
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (featurep 'olivetti)
           (olivetti-reset-all-windows))))

(defface olivetti-fringe
  '((t (:inherit fringe)))
  "Face for the fringes when `olivetti-style' is non-nil."
  :group 'olivetti)


;;; Set Windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun olivetti-scale-width (width)
  "Scale WIDTH in accordance with the face height.
For compatibility with `text-scale-mode', if
`face-remapping-alist' includes a :height property on the default
face, scale WIDTH by that factor if it is a fraction, by (height/100)
if it is an integer, and otherwise return WIDTH."
  (let ((height (plist-get (cadr (assq 'default face-remapping-alist)) :height)))
    (when (integerp height)
      (setq height (/ height 100.0)))
    (round (* width (or height 1)))))

(defun olivetti-normalize-width (width window)
  "Parse WIDTH to a safe pixel value for `olivetti-body-width' for WINDOW."
  (let ((char-width (frame-char-width (window-frame window)))
        (window-width-pix (window-body-width window t))
        min-width-pix)
    (setq min-width-pix (* char-width
                           (+ olivetti-minimum-body-width
                              (% olivetti-minimum-body-width 2))))
    (olivetti-scale-width
     (if (floatp width)
         (floor (max min-width-pix (* window-width-pix (min width 1.0))))
       (max min-width-pix (min (* width char-width) window-width-pix))))))

(defun olivetti-reset-window (window)
  "Remove Olivetti's parameters and margins from WINDOW."
  (when (eq (window-parameter window 'split-window) 'olivetti-split-window)
    (set-window-parameter window 'split-window nil))
  (if (consp fringe-mode)
      (set-window-fringes window (car fringe-mode) (cdr fringe-mode))
    (set-window-fringes window fringe-mode fringe-mode))
  (set-window-margins window nil))

(defun olivetti-reset-all-windows ()
  "Call `olivetti-reset-window' on all windows."
  (walk-windows #'olivetti-reset-window nil t))

;; FIXME: these split-window functions seem to be ignored by
;; `window-toggle-side-windows'
;; WORKAROUND:
;; (with-eval-after-load 'olivetti
;;   (advice-add 'window-toggle-side-windows
;;               :before 'olivetti-reset-all-windows))

(defun olivetti-split-window (&optional window size side pixelwise)
  "Call `split-window' after resetting WINDOW.
Pass SIZE, SIDE and PIXELWISE unchanged."
  (olivetti-reset-all-windows)
  (split-window window size side pixelwise))

(defun olivetti-split-window-sensibly (&optional window)
  "Like `olivetti-split-window' but call `split-window-sensibly'.
Pass WINDOW unchanged."
  (olivetti-reset-all-windows)
  (funcall olivetti--split-window-preferred-function window))

(defun olivetti-set-window (window-or-frame)
  "Balance window margins displaying current buffer.
If WINDOW-OR-FRAME is a frame, cycle through windows displaying
current buffer in that frame, otherwise only work on the selected
window."
  (if (framep window-or-frame)
      (mapc #'olivetti-set-window
            (get-buffer-window-list nil nil window-or-frame))
    ;; WINDOW-OR-FRAME passed below *must* be a window
    (with-selected-window window-or-frame
      (olivetti-reset-window window-or-frame)
      (when olivetti-mode
        ;; If `olivetti-body-width' is nil, we need to calculate from
        ;; `fill-column'
        (when (null olivetti-body-width)
          (setq olivetti-body-width (+ fill-column 2)))
        (let ((char-width-pix   (frame-char-width (window-frame window-or-frame)))
              (window-width-pix (window-body-width window-or-frame t))
              (safe-width-pix   (olivetti-normalize-width
                                 olivetti-body-width window-or-frame)))
          ;; Handle possible display of fringes
          (when (and window-system olivetti-style)
            (let ((fringe-total (- (window-pixel-width window-or-frame)
                                   safe-width-pix))
                  fringe)
              ;; Account for fancy display
              (when (eq olivetti-style 'fancy)
                (setq fringe-total
                      (- fringe-total
                         (* olivetti-margin-width char-width-pix 2))))
              ;; Calculate a single fringe width
              (setq fringe (max (round (/ fringe-total 2.0)) 0))
              ;; Set the fringes
              (set-window-fringes window-or-frame fringe fringe t)))
          ;; Calculate margins widths as body pixel width less fringes
          (let ((fringes (window-fringes window-or-frame))
                (margin-total-pix (/ (- window-width-pix safe-width-pix) 2.0))
                left-margin right-margin)
            ;; Convert to character cell columns
            (setq left-margin  (max (round (/ (- margin-total-pix
                                                 (car fringes))
                                              char-width-pix))
                                    0)
                  right-margin (max (round (/ (- margin-total-pix
                                                 (cadr fringes))
                                              char-width-pix))
                                    0))
            ;; Finally set the margins
            (set-window-margins window-or-frame left-margin right-margin)))
        ;; Set remaining window parameters
        (set-window-parameter window-or-frame 'split-window
                              'olivetti-split-window)))))

(defun olivetti-set-buffer-windows ()
  "Balance window margins in all windows displaying current buffer.
Cycle through all windows in all visible frames displaying the
current buffer, and call `olivetti-set-window'."
  (mapc #'olivetti-set-window (get-buffer-window-list nil nil 'visible)))


;;; Width Interaction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun olivetti-set-width (width)
  "Set text body width to WIDTH with relative margins.
WIDTH may be an integer specifying columns or a float specifying
a fraction of the window width."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Set text body width (integer or float): "
                        olivetti-body-width))))
  (setq olivetti-body-width width)
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
    (setq olivetti-body-width n))
  (olivetti-set-buffer-windows)
  (message "Text body width set to %s" olivetti-body-width)
  (unless overriding-terminal-local-map
    (let ((prefix-keys (substring (this-single-command-keys) 0 -1))
          (map (cdr olivetti-mode-map)))
      (when (< 0 (length prefix-keys))
        (mapc (lambda (k) (setq map (assq k map))) prefix-keys)
        (setq map (cdr-safe map))
        (when (keymapp map) (set-transient-map map t))))))

(defun olivetti-shrink (&optional arg)
  "Incrementally decrease the value of `olivetti-body-width'.
If prefixed with ARG, incrementally increase."
  (interactive "P")
  (let ((p (unless arg t)))
    (olivetti-expand p)))


;;; Keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar olivetti-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c }") #'olivetti-expand)
    (define-key map (kbd "C-c {") #'olivetti-shrink)
    (define-key map (kbd "C-c |") #'olivetti-set-width)
    (define-key map (kbd "C-c \\") #'olivetti-set-width) ;; OBSOLETE
    (define-key map [left-margin mouse-1] #'mouse-set-point)
    (define-key map [right-margin mouse-1] #'mouse-set-point)
    (define-key map [left-fringe mouse-1] #'mouse-set-point)
    (define-key map [right-fringe mouse-1] #'mouse-set-point)
    ;; This code is taken from https://github.com/joostkremers/visual-fill-column
    (when (and (bound-and-true-p mouse-wheel-mode)
               (boundp 'mouse-wheel-down-event)
               (boundp 'mouse-wheel-up-event))
      (define-key map (vector 'left-margin 'mouse-wheel-down-event) 'mwheel-scroll)
      (define-key map (vector 'left-margin 'mouse-wheel-up-event) 'mwheel-scroll)
      (define-key map (vector 'right-margin 'mouse-wheel-down-event) 'mwheel-scroll)
      (define-key map (vector 'right-margin 'mouse-wheel-up-event) 'mwheel-scroll))
    map)
  "Mode map for `olivetti-mode'.")


;;; Mode Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'face-remap)

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
        (unless (bound-and-true-p olivetti--visual-line-mode)
          (setq olivetti--visual-line-mode
                visual-line-mode))
        (unless (bound-and-true-p olivetti--split-window-preferred-function)
          (setq olivetti--split-window-preferred-function
                split-window-preferred-function))
        (setq-local split-window-preferred-function
                    #'olivetti-split-window-sensibly)
        (setq olivetti--face-remap
              (face-remap-add-relative 'fringe 'olivetti-fringe))
        (olivetti-set-buffer-windows))
    (remove-hook 'window-configuration-change-hook
                 #'olivetti-set-buffer-windows t)
    (remove-hook 'window-size-change-functions
                 #'olivetti-set-window t)
    (remove-hook 'text-scale-mode-hook
                 #'olivetti-set-window t)
    (olivetti-set-buffer-windows)
    (when olivetti--face-remap
      (face-remap-remove-relative olivetti--face-remap))
    (when olivetti-recall-visual-line-mode-entry-state
      (if olivetti--visual-line-mode
          (when (not visual-line-mode) (visual-line-mode 1))
        (when visual-line-mode (visual-line-mode 0))))
    (mapc #'kill-local-variable '(split-window-preferred-function
                                  olivetti-body-width
                                  olivetti--visual-line-mode
                                  olivetti--face-remap
                                  olivetti--split-window-preferred-function))))



(provide 'olivetti)

;;; olivetti.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
