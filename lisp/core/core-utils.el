;;; core-utils.el --- Utilities for Emacs initialization -*- lexical-binding: t; -*-

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

(defvar core-utils-after-startup-time nil
  "Value of `current-time' after `after-init' hooks.")

(defvar core-utils-gcs-done-during-init nil
  "Number of garbage collections done after the Emacs initialization.")

(defvar core-utils-gcs-done-during-startup nil
  "Number of garbage collections done after the Emacs startup.")

(defvar core-utils-first-interaction-hook nil
  "Normal hook run before first interaction in the Emacs session.
The first interaction is considered to be the first command call
or first `find-file' non-interactive call.")

(defun core-utils-benchmark ()
  "Return benchmark of the Emacs startup."
  (interactive)
  (let ((init-time (float-time (time-subtract after-init-time before-init-time)))
	(startup-time (float-time (time-subtract core-utils-after-startup-time before-init-time))))
    (message (concat "         Emacs startup time         \n"
	             "                                    \n"
	             "| stage       | time [s] | gcs     |\n"
	             "+-------------+----------+---------+\n"
	             "| init:       | %.2e | %.1e |\n"
	             "| after init: | %.2e | %.1e |\n"
	             "+-------------+----------+---------+\n"
	             "| startup:    | %.2e | %.1e |")
             init-time
             core-utils-gcs-done-during-init
             (- startup-time init-time)
             (- core-utils-gcs-done-during-startup core-utils-gcs-done-during-init)
             startup-time
             core-utils-gcs-done-during-startup)))

(defun core-utils-check-emacs-version (ver)
  "Give warning about old Emacs if `emacs-version' is lower than VER."
  (when (version< emacs-version ver)
    (warn "%s %s\n  %s %s %s"
          "Your Emacs version is" emacs-version
	  "this configuration was only tested with version" ver "or higher")))

(defun core-utils-run-first-interaction-hook ()
  "Run `core-utils-first-interaction-hook'."
  (run-hooks 'core-utils-first-interaction-hook)
  (remove-hook 'find-file-hook #'core-utils-run-first-interaction-hook)
  (remove-hook 'pre-command-hook #'core-utils-run-first-interaction-hook))

(defun core-utils-set-after-startup-time ()
  "Set `core-utils-after-startup-time' value to the value of `current-time'."
  (unless core-utils-after-startup-time
    (setq core-utils-after-startup-time (current-time))))

(defun core-utils-set-gc-cons-threshold (th &optional init-th)
  "Set `gc-cons-threshold' value equal to TH.
If INIT-TH is provided then set its value for the initialization
and change to TH value after the Emacs startup."
  (if init-th
      (progn (setq gc-cons-threshold init-th)
	     (add-hook 'emacs-startup-hook
		       (lambda () (setq gc-cons-threshold th))))
    (setq gc-cons-threshold th)))

(defun core-utils-set-init-gcs-done ()
  "Set `core-utils-gcs-done-during-init' value to the current value of `gcs-done'."
  (unless core-utils-gcs-done-during-init
    (setq core-utils-gcs-done-during-init gcs-done)))

(defun core-utils-set-startup-gcs-done ()
  "Set `core-utils-gcs-done-during-startup' value to the current value of `gcs-done'."
  (unless core-utils-gcs-done-during-startup
    (setq core-utils-gcs-done-during-startup gcs-done)))

(add-hook 'after-init-hook #'core-utils-set-init-gcs-done -100)
(add-hook 'emacs-startup-hook #'core-utils-benchmark 100)
(add-hook 'emacs-startup-hook #'core-utils-set-after-startup-time 99)
(add-hook 'emacs-startup-hook #'core-utils-set-startup-gcs-done 99)
(add-hook 'find-file-hook #'core-utils-run-first-interaction-hook -50)
(add-hook 'pre-command-hook #'core-utils-run-first-interaction-hook -50)

(setq gc-cons-percentage 1.0)

(core-utils-check-emacs-version "27.2")

(core-utils-set-gc-cons-threshold 1073741824 ; (* 1 1024 1024 1024)
			          most-positive-fixnum)

;;; Footer:

(provide 'core-utils)

;;; core-utils.el ends here
