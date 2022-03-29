;;; init-startup.el --- startup configuration -*- lexical-binding: t -*-

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

;; `initial-major-mode' can be set to change the major
;; mode of the =*scratch*= buffer.

;; (setq initial-major-mode 'text-mode)

;; Changing content of the message for the scratch buffer.

(setq initial-scratch-message ";; Scratch\n\n")

;; Preventing startup screen for displaying as the initial screen for Emacs
;; session.

(setq inhibit-startup-screen t)

(provide 'init-startup)

;;; init-startup.el ends here
