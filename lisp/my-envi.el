;;; my-envi.el --- Environment configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://github.com/rafalraczka/emacs-config

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

(defconst my-envi-daemon (daemonp))

;;;; Paths ----------------------------

(defconst my-envi-user-files-directory
  (if my-envi-android
      "~/storage/shared/"
    "~/")
  "Default directory where user stores files.")

(defun my-envi-concat-library-file-directory (path)
  (concat (file-name-directory path) "files/"))

(defconst my-envi-library-directory
  (let ((dir (expand-file-name "library/" my-envi-user-files-directory)))
    (when (file-exists-p dir)
      dir))
  "Default directory where user stores publications and its metadata.")

(defconst my-envi-bib-files
  (when my-envi-library-directory
    (directory-files-recursively my-envi-library-directory "\\.bib$")))

(defconst my-envi-bib-files-directories
  (when my-envi-library-directory
    (mapcar 'my-envi-concat-library-file-directory my-envi-bib-files)))

(defconst my-envi-chemacs-directory
  (when-let ((file (car (rassoc '((require . chemacs)) load-history))))
    (file-name-directory file))
  "This constant specify directory from which chemacs.el has been required.
If Chemacs is not in use in current session the constant value is
nil.")

(defconst my-envi-org-directory
  (expand-file-name "org" my-envi-user-files-directory)
  "Default directory where user stores org files.")

(defconst my-envi-projects-directory
  (list (expand-file-name "projects/" my-envi-user-files-directory))
  "Default directory with projects.")

(defconst my-envi-ref-notes-directory
  (expand-file-name "references" my-envi-org-directory))

;;;; Others ---------------------------


(defun my-envi-exwm-require-p ()
  (and (member "--use-exwm" command-line-args)
       (eq window-system 'x)))

(defconst my-envi-exwm-required (my-envi-exwm-require-p))

(provide 'my-envi)

;;; my-envi.el ends here
