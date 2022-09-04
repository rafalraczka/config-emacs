;;; my-etc.el --- miscellaneous tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://codeberg.org/rafalraczka/emacs-config

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

;;;###autoload
(defun my-etc-blink-mode-line-bg (&optional color)
  (let ((blink-color (or color "#762422"))
        (original-color (face-background 'mode-line)))
    (set-face-background 'mode-line blink-color)
    (run-with-idle-timer 0.1 nil
                         (lambda (col) (set-face-background 'mode-line col))
                         original-color)))

;;;###autoload
(defun my-etc-delete-window-dwim ()
  "Delete current window or tab if the window cannot be deleted."
  (interactive)
  (or (ignore-error t (or (delete-window) t))
      (ignore-error t (or (tab-bar-close-tab) t))
      (when-let* ((dashboard (or (when (featurep 'pico-dashboard)
                                   pico-dashboard-buffer-name)
                                 "*dashboard*"))
                  (buffer (get-buffer dashboard)))
        (switch-to-buffer buffer))
      (t (switch-to-buffer "*scratch*"))))

;;;###autoload
(defun my-etc-tab-bar-switch-tab-dwim ()
  "Switch to the tab by NAME or do other action if there are fewer tabs.
If there is only one tab this function will create a new tab.  If
there are two tabs, this function will switch to the other tab
without prompt.  If there are more than two tabs than, prompt
will appear for user to select a tab."
  (interactive)
  (pcase (length (tab-bar-tabs))
    (1 (tab-bar-new-tab))
    (2 (tab-bar-switch-to-next-tab))
    (_ (call-interactively 'tab-bar-switch-to-tab))))

;;;###autoload
(defun my-etc-transpose-paragraphs-previous (arg)
  (interactive "*p")
  (setq arg (if arg (- arg) -1))
  (transpose-paragraphs arg))

;;;###autoload
(defun my-etc-upcase-previous-word ()
  "Convert to upper case from point to the beginning of word and do not move."
  (interactive)
  (upcase-word -1))

;;; Footer:

(provide 'my-etc)

;;; my-etc.el ends here
