;;; my-polybar.el --- Polybar tools -*- lexical-binding: t; -*-

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

(defvar my-polybar-panel-process nil)

;;;###autoload
(defun my-polybar-kill-panel ()
  (interactive)
  (when my-polybar-panel-process
    (ignore-errors
      (kill-process my-polybar-panel-process)))
  (setq my-polybar-panel-process nil))

;;;###autoload
(defun my-polybar-start-panel ()
  (interactive)
  (my-polybar-kill-panel)
  (setq my-polybar-panel-process (start-process-shell-command "polybar" nil "polybar panel")))

;;;###autoload
(defun my-polybar-toggle-panel ()
  "Enable polybar if disabled or disable if enabled."
  (interactive)
  (if my-polybar-panel-process
      (my-polybar-kill-panel)
    (my-polybar-start-panel)))

;;; Footer:

(provide 'my-polybar)

;;; my-polybar.el ends here
