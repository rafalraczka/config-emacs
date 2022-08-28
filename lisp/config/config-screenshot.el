;;; config-screenshot.el --- screenshot configuration -*- lexical-binding: t; -*-

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

(require 'smartparens)
(require 'flyspell)

(setq screenshot-border-width 0)
(setq screenshot-shadow-offset-horizontal 0)
(setq screenshot-shadow-offset-vertical 0)
(setq screenshot-shadow-radius 0)

(defun my/screenshot-disable-flyspell-mode ()
  (flyspell-mode -1))

(defun my/screenshot-disable-show-smartparens-mode ()
  (show-smartparens-mode -1))

(add-hook 'screenshot-buffer-creation-hook
          #'my/screenshot-disable-flyspell-mode)

(add-hook 'screenshot-buffer-creation-hook
          #'my/screenshot-disable-show-smartparens-mode)

;;; Footer:

(provide 'config-screenshot)

;;; config-screenshot.el ends here
