;;; config-pico-dashboard.el --- pico-dashboard configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>

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

(defun config-pico-dashboard-set-server-footer ()
  (if (server-running-p)
      (setq pico-dashboard-set-footer nil)
    (setq pico-dashboard-footer-messages (list "standalone"))))

(add-hook 'after-init-hook #'config-pico-dashboard-set-server-footer)

(setq pico-dashboard-banner-logo-title nil)
(setq pico-dashboard-items
      '(
        (:description "Agenda" :key "a" :function org-agenda)
        (:description "Bookmark" :key "b" :function bookmark-jump)
        (:description "Mail" :key "m" :function my-keymap-mail-map)
        (:description "Project" :key "p" :function projectile-switch-project)
        ))

(if core-envi-android
    (setq pico-dashboard-image-banner-max-height 50)
  (setq pico-dashboard-image-banner-max-height 200))

(let ((command-line-args '(only-one)))
  (pico-dashboard-setup-startup-hook))

;;; Footer:

(provide 'config-pico-dashboard)

;;; config-pico-dashboard.el ends here
