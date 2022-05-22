;;; init-text-mode.el --- text-mode configuration -*- lexical-binding: t -*-

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

(with-eval-after-load 'text-mode

  (defcustom my/text-mode-fill-column 72
    "Default `fill-column' for Text mode."
    :type 'integer)

  (defun my/text-mode-config ()
    (setq-local fill-column my/text-mode-fill-column))

  (add-hook 'text-mode-hook 'my/text-mode-config)

  )

(provide 'init-text-mode)

;;; init-text-mode.el ends here
