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

(defcustom olivetti-body-width 66
  "Text body width in columns to which to adjust margin width.

Does not affect file contents."
  :type 'integer
  :group 'olivetti)

(defcustom olivetti-hide-mode-line t
  "Hide the mode line."
  :type 'boolean
  :group 'olivetti)

(defun olivetti-set-window-margins ()
  "Set window-body-width to `olivetti-body-width' with relative margins."
  (let ((margin
         (/ (- (window-total-width) olivetti-body-width) 2)))
    (set-window-margins (selected-window) margin margin)))

;; (easy-menu-define olivetti-mode-menu olivetti-mode-map
;;   "Menu for Olivetti Mode."
;;   '("Olivetti"
;;     ["Use Clean Margins" ignore]
;;     ["Hide Mode Line" ignore
;;      :style toggle
;;      :selected olivetti-hide-mode-line]))

;; (defvar olivetti-mode-map nil
;;   "Mode map for `olivetti-mode'.")

;;;###autoload
(defun turn-on-olivetti-mode ()
  "Turn on `olivetti-mode' unconditionally."
  (interactive)
  (olivetti-mode 1))

;;;###autoload
(define-minor-mode olivetti-mode
  ""
  :init-value nil
  :lighter " Olv"
  (if olivetti-mode
      (progn
        (setq-local scroll-conservatively 101)
        (when olivetti-hide-mode-line
          (setq-local mode-line-format nil))
        (add-hook 'window-configuration-change-hook
                  'olivetti-set-window-margins nil t)
        (run-hook-with-args 'window-configuration-change-hook))
    (kill-local-variable 'mode-line-format)
    (remove-hook 'window-configuration-change-hook
                 'olivetti-set-window-margins t)
    (set-window-margins nil nil)))

(provide 'olivetti)
;;; olivetti.el ends here
