;;; config-org-roam.el --- org-roam configuration -*- lexical-binding: t; -*-

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

(require 'org-roam-overlay)

(defun my/org-roam-buffer-startup ()
  (when (and (org-roam-buffer-p) (not (buffer-narrowed-p)))
    (setq org-startup-folded 'show2levels)
    (org-set-startup-visibility)))

(add-hook 'org-mode-hook #'my/org-roam-buffer-startup)
(add-hook 'org-mode-hook #'org-roam-db-autosync-mode)

(push '("\\*org-roam\\*"
        (display-buffer-in-direction)
        (direction . right)
        (window-width . 0.33)
        (window-height . fit-window-to-buffer))
      display-buffer-alist)

(setq org-roam-completion-everywhere t)
(setq org-roam-dailies-directory "journal/")
(setq org-roam-db-extra-links-exclude-keys '((node-property . ("ROAM_REFS"))))
(setq org-roam-directory org-directory)

;;; Footer:

(provide 'config-org-roam)

;;; config-org-roam.el ends here
