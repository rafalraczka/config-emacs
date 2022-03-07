;;; my-envi.el --- Environment configuration -*- lexical-binding: t -*-

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

;;;; System dependent variables -------

(defun my-envi-android-p ()
  "Return t if current system is Android, and return nil otherwise."
  (string-equal "Android\n" (shell-command-to-string "uname -o")))

(defun my-envi-gnu-linux-p ()
  "Return t if current system is GNU/Linux, and return nil otherwise."
  (eq system-type 'gnu/linux))

(defun my-envi-windows-p ()
  "Return t if current system is Windows, and return nil otherwise."
  (eq system-type 'windows-nt))

(defconst my-envi-android (my-envi-android-p)
  "This constant specify whatever user is using Emacs on device with Android system.")

(defconst my-envi-gnu-linux (my-envi-gnu-linux-p)
  "This constant specify whatever user is using Emacs on device with GNU/Linux system.")

(defconst my-envi-windows (my-envi-windows-p)
  "This constant specify whatever user is using Emacs on device with Windows system.")

(provide 'my-envi)

;;; my-envi.el ends here
