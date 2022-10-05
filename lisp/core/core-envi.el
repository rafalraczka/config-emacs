;;; core-envi.el --- Environment configuration -*- lexical-binding: t; -*-

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

;;;; System dependent variables -------

(defun core-envi-android-p ()
  "Return t if current system is Android, and return nil otherwise."
  (string-equal "Android\n" (shell-command-to-string "uname -o")))

(defun core-envi-gnu-linux-p ()
  "Return t if current system is GNU/Linux, and return nil otherwise."
  (eq system-type 'gnu/linux))

(defun core-envi-windows-p ()
  "Return t if current system is Windows, and return nil otherwise."
  (eq system-type 'windows-nt))

(defconst core-envi-android (core-envi-android-p)
  "This constant specify whatever user is using Emacs on device with Android system.")

(defconst core-envi-gnu-linux (core-envi-gnu-linux-p)
  "This constant specify whatever user is using Emacs on device with GNU/Linux system.")

(defconst core-envi-windows (core-envi-windows-p)
  "This constant specify whatever user is using Emacs on device with Windows system.")

(defconst core-envi-daemon (daemonp))

;;;; Paths ----------------------------

(defconst core-envi-user-files-directory
  (if core-envi-android
      "~/storage/shared/"
    "~/")
  "Default directory where user stores files.")

(defun core-envi-concat-library-file-directory (path)
  (concat (file-name-directory path) "files/"))

(defconst core-envi-library-directory
  (let ((dir (expand-file-name "library/" core-envi-user-files-directory)))
    (when (file-exists-p dir)
      dir))
  "Default directory where user stores publications and its metadata.")

(defconst core-envi-bib-files
  (when core-envi-library-directory
    (directory-files-recursively core-envi-library-directory "\\.bib$")))

(defconst core-envi-bib-files-directories
  (when core-envi-library-directory
    (mapcar 'core-envi-concat-library-file-directory core-envi-bib-files)))

(defconst core-envi-chemacs-directory
  (when-let ((file (car (rassoc '((require . chemacs)) load-history))))
    (file-name-directory file))
  "This constant specify directory from which chemacs.el has been required.
If Chemacs is not in use in current session the constant value is
nil.")

(defconst core-envi-org-directory
  (expand-file-name "org" core-envi-user-files-directory)
  "Default directory where user stores org files.")

(defconst core-envi-projects-directory
  (let ((cmd "xdg-user-dir"))
    (if (executable-find cmd)
        (string-trim (shell-command-to-string (concat cmd " PROJECTS")))
      (expand-file-name "prj/" core-envi-user-files-directory)))
  "Default directory with projects.")

(defconst core-envi-ref-notes-directory
  (expand-file-name "references" core-envi-org-directory))

;;;; Others ---------------------------


(defun core-envi-exwm-require-p ()
  (and (member "--use-exwm" command-line-args)
       (eq window-system 'x)))

(defconst core-envi-exwm-required (core-envi-exwm-require-p))

;;; Footer:

(provide 'core-envi)

;;; core-envi.el ends here
