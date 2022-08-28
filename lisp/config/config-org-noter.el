;;; config-org-noter.el --- org-noter configuration -*- lexical-binding: t; -*-

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

(require 'org-noter)

(setq org-noter-always-create-frame nil)
(setq org-noter-auto-save-last-location t)
(setq org-noter-doc-split-fraction '(0.6 0.4))
(setq org-noter-kill-frame-at-session-end nil)
(setq org-noter-notes-search-path (list core-envi-ref-notes-directory))

;;; Footer:

(provide 'config-org-noter)

;;; config-org-noter.el ends here
