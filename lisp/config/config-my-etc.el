;;; config-my-etc.el --- miscellaneous configurations -*- lexical-binding: t; -*-

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

(advice-add 'yes-or-no-p :override 'y-or-n-p)

(add-hook 'core-utils-first-interaction-hook #'column-number-mode)

(setq delete-by-moving-to-trash t)
(setq ring-bell-function 'my-etc-blink-mode-line-bg)

;;; Footer:

(provide 'config-my-etc)

;;; config-my-etc.el ends here
