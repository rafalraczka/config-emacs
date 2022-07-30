;;; config-package.el --- package configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://git.sr.ht/~rafalraczka/emacs-config

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

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar my/package-contents-refreshed nil)

(defun my/package-ensure (package)
  "Ensure that PACKAGE is installed and return non-nil if successful."
  (or (package-installed-p package)
      (progn (if (assoc package package-archive-contents)
                 (condition-case err
                     (package-install package)
                   (error (my/package-refresh-maybe-and-install package)))
               (my/package-refresh-maybe-and-install package))
             (package-installed-p package))))

(defun my/package-refresh-maybe-and-install (package)
  "Refresh archive contents if not done yet and install PACKAGE."
  (when (not my/package-contents-refreshed)
    (package-refresh-contents)
    (setq my/package-contents-refreshed t))
  (package-install package))

(package-initialize)

;;; Footer:

(provide 'config-package)

;;; config-package.el ends here
