;;; init-tab-bar.el --- tab-bar configuration -*- lexical-binding: t -*-

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

(with-eval-after-load 'tab-bar
  (setq tab-bar-back-button nil)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-forward-button nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-show 1)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (setq tab-bar-tab-name-truncated-max 10))

(provide 'init-tab-bar)

;;; init-tab-bar.el ends here
