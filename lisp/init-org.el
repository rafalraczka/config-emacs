;;; init-org.el --- org configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka

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

(straight-use-package 'org)

(with-eval-after-load 'org
  (defcustom my/org-fill-column 72
    "Default `fill-column' for Org mode."
    :type 'integer)

  (defun my/org-mode-config ()
    (setq fill-column my/org-fill-column))

  (add-hook 'org-mode-hook #'my/org-mode-config)
  )

(provide 'init-org)

;;; init-org.el ends here
