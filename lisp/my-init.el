;;; my-init.el --- Utilities for Emacs initialization -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://github.com/rafalraczka/config-emacs

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar my-init-after-startup-time nil
  "Value of `current-time' after `after-init' hooks.")

(defvar my-init-gcs-done-during-init nil
  "Number of garbage collections done after the Emacs initialization.")

(defvar my-init-gcs-done-during-startup nil
  "Number of garbage collections done after the Emacs startup.")

(defun my-init-benchmark ()
  "Return benchmark of the Emacs startup."
  (interactive)
  (let ((init-time (float-time (time-subtract after-init-time before-init-time)))
	(startup-time (float-time (time-subtract my-init-after-startup-time before-init-time))))
    (message "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s"
	     "         Emacs startup time         "
	     "                                    "
	     "| stage       | time [s] |   gcs   |"
	     "+-------------+----------+---------+"
	   (format "| init:       | %.2e | %.1e |"
		   init-time
		   my-init-gcs-done-during-init)
	   (format "| after init: | %.2e | %.1e |"
		   (- startup-time init-time)
		   (- my-init-gcs-done-during-startup my-init-gcs-done-during-init))
	   "+-------------+----------+---------+"
	   (format "| startup:    | %.2e | %.1e |"
		   startup-time
		   my-init-gcs-done-during-startup))))

(defun my-init-check-emacs-version (ver)
  "Give warning about old Emacs if `emacs-version' is lower than VER."
  (when (version< emacs-version ver)
    (warn "%s %s\n  %s %s %s"
          "Your Emacs version is" emacs-version
	  "this configuration was only tested with version" ver "or higher")))

(defun my-init-set-after-startup-time ()
  "Set `my-init-after-startup-time' value to the value of `current-time'."
  (unless my-init-after-startup-time
    (setq my-init-after-startup-time (current-time))))

(defun my-init-set-gc-cons-threshold (th &optional init-th)
  "Set `gc-cons-threshold' value equal to TH.
If INIT-TH is provided then set its value for the initialization
and change to TH value after the Emacs startup."
  (if init-th
      (progn (setq gc-cons-threshold init-th)
	     (add-hook 'emacs-startup-hook
		       (lambda () (setq gc-cons-threshold th))))
    (setq gc-cons-threshold th)))

(defun my-init-set-init-gcs-done ()
  "Set `my-init-gcs-done-during-init' value to the current value of `gcs-done'."
  (unless my-init-gcs-done-during-init
    (setq my-init-gcs-done-during-init gcs-done)))

(defun my-init-set-startup-gcs-done ()
  "Set `my-init-gcs-done-during-startup' value to the current value of `gcs-done'."
  (unless my-init-gcs-done-during-startup
    (setq my-init-gcs-done-during-startup gcs-done)))

(add-hook 'after-init-hook 'my-init-set-init-gcs-done -100)
(add-hook 'emacs-startup-hook 'my-init-benchmark 100)
(add-hook 'emacs-startup-hook 'my-init-set-startup-gcs-done 99)
(add-hook 'emacs-startup-hook 'my-init-set-after-startup-time 99)

(my-init-check-emacs-version "27.2")

(my-init-set-gc-cons-threshold 33554432 ; (* 32 1024 1024)
			       134217728) ; (* 128 1024 1024)

(provide 'my-init)

;;; my-init.el ends here
