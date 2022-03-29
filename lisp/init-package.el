;;; init-package.el --- package configuration -*- lexical-binding: t -*-

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

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar my/package-contents-refreshed nil)

(defun my/package-ensure (package)
  "Ensure that PACKAGE is installed and return non-nil if successful."
  (or (package-installed-p package)
      (let* ((archive (cdr (assoc package package-archive-contents)))
             (newest (car (sort archive (lambda (a b)
                                          (version-list-<= (package-desc-version b)
                                                           (package-desc-version a)))))))
        (if newest
            (package-install newest)
          (when (not my/package-contents-refreshed)
            (package-refresh-contents)
            (setq my/package-contents-refreshed t)
            (my/package-ensure package)))
        (package-installed-p package))))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(provide 'init-package)

;;; init-package.el ends here
