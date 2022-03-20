;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-init)
(require 'init-straight)
(require 'init-package)

(require 'my-envi)

(require 'init-calendar)
(require 'init-custom)
(require 'init-ess)
(require 'init-helpful)
(require 'init-magit)

(when (not my-envi-android)
  (require 'init-olivetti))

(require 'init-org)
(require 'init-selectrum)
(require 'init-tab-bar)
(require 'init-undo-tree)
(require 'init-which-key)

(provide 'init)

;;; init.el ends here
