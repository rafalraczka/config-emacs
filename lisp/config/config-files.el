;;; config-files.el --- files configuration -*- lexical-binding: t; -*-

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

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; Emacs create backups and auto-save files in directories of that files by
;; default.  It can cause cluttering and it can require additional work to
;; configure different software to ignore backup files and auto-saves.
;; Because of that it can be desired then to keep these files in single
;; directory, independent of file location.

;; Following code will keep all backup and auto-save files in directory of the
;; Emacs configuration.  In case user use Chemacs, Chemacs directory will be
;; preferred to keep auto-saves and backups in the same place for various
;; Emacs profiles.

;; Sources
;; - https://emacstragic.net/uncategorized/editing-files-using-sudo-and-emacs/

(let ((dir (cond (core-envi-chemacs-directory)
		 (user-emacs-directory))))
  (setq auto-save-file-name-transforms`((".*" ,(concat dir "auto-saves/") t)))
  (setq backup-directory-alist `(("." . ,(concat dir "backups/")))))

;; Create directories for auto-save files.

(mapcar (lambda (a)
          (let ((dir (nth 1 a)))
            (when (not (file-exists-p dir))
              (mkdir dir))))
        auto-save-file-name-transforms)

;;; Footer:

(provide 'config-files)

;;; config-files.el ends here
