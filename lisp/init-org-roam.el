;;; init-org-roam.el --- org-roam configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://github.com/rafalraczka/emacs-config
;; Keywords: emacs, org, org-roam

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

(straight-use-package
 '(org-roam :types git
            :flavor melpa
            :files (:defaults "extensions/*" "org-roam-pkg.el")
            :host github
            :repo "org-roam/org-roam"
            :fork (:repo "rafalraczka/org-roam")))

(unless (file-exists-p (expand-file-name "org-roam.db" user-emacs-directory))
  (add-hook 'after-init-hook 'org-roam-db-sync))

(with-eval-after-load 'org

  (add-hook 'org-mode-hook #'org-roam-db-autosync-mode)

  )

(provide 'init-org-roam)

;;; init-org-roam.el ends here
