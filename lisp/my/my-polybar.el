;;; my-polybar.el --- Polybar tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Rafał Rączka <info@rafalraczka.com>

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

(require 'xdg)

(defvar my-polybar-base-command "polybar")
(defvar my-polybar-config-home-directory
  (expand-file-name "polybar" (xdg-config-home)))
(defvar my-polybar-config-file-path nil)
(defvar my-polybar-launch-script-filename "launch.sh")
(defvar my-polybar-launch-script-path nil)
(defvar my-polybar-bar-name nil)
(defvar my-polybar-process nil)

(defun my-polybar--start-shell-command (&optional bar-name)
  "Get the command to start polybar."
  (cond
   ((let ((launch-script-path (or my-polybar-launch-script-path
                                  (expand-file-name
                                   my-polybar-launch-script-filename
                                   my-polybar-config-home-directory))))
      (when (ignore-errors (file-executable-p launch-script-path))
        launch-script-path)))
   ((executable-find "polybar")
    (let ((base-cmd (if my-polybar-config-file-path
                        (format "%s --config=\"%s\""
                                my-polybar-base-command
                                my-polybar-config-file-path)
                      my-polybar-base-command)))
      (if-let ((bar-name (or bar-name my-polybar-bar-name)))
          (format "%s %s" base-cmd bar-name)
        base-cmd)))
   (t (error "There is no command available to start polybar"))))

;;;###autoload
(defun my-polybar-kill ()
  "Kill "
  (interactive)
  (if my-polybar-process
      (prog2
        (ignore-errors
          (kill-process my-polybar-process))
          my-polybar-process
        (setq my-polybar-process nil))
    (user-error "No polybar process or polybar not started with my-polybar")))

;;;###autoload
(defun my-polybar-start ()
  (interactive)
  (let ((cmd (my-polybar--start-shell-command)))
    (when my-polybar-process
      (my-polybar-kill))
    (setq my-polybar-process (start-process-shell-command
                                    "polybar" nil cmd))))

;;;###autoload
(defalias 'my-polybar-restart 'my-polybar-start)

;;;###autoload
(defun my-polybar-toggle-panel ()
  "Enable polybar if disabled or disable if enabled."
  (interactive)
  (if my-polybar-process
      (my-polybar-kill)
    (my-polybar-start)))

;;; Footer:

(provide 'my-polybar)

;;; my-polybar.el ends here
