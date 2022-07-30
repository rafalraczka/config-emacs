;;; config-notmuch.el --- notmuch configuration -*- lexical-binding: t; -*-

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

(my/package-ensure 'notmuch)

(with-eval-after-load 'notmuch

  (setq notmuch-saved-searches
        '(
          (:name "all mail" :query "*" :key "a")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "today" :query "date:today" :key "t")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "week (this)" :query "date:\"this week\"" :key "w")
        ))
  )

;;; Footer:

(provide 'config-notmuch)

;;; config-notmuch.el ends here
