;;; config-org-super-agenda.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://git.sr.ht/~rafalraczka/emacs-config

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

(my/package-ensure 'org-super-agenda)

(with-eval-after-load 'org-agenda

    (setq my/org-super-agenda-groups
        '(
          (:name "! Important"
                 :priority>= ("B")
                 :order 1)
          (:name "! Overdue"
                 :deadline past
                 :order 5)
          (:name "! Behind schedule"
                 :scheduled past
                 :order 8)
          (:name "Today"
                 :deadline today
                 :scheduled today
                 :order 10)
          (:name "Unfinished"
                 :todo "STRT"
                 :order 15)
          (:name "Waiting"
                 :todo "WAIT"
                 :order 60)
          ))
  (setq org-super-agenda-unmatched-name "Other items")
  (setq org-super-agenda-unmatched-order 50)

  )

(provide 'config-org-super-agenda)

;;; config-org-super-agenda.el ends here
