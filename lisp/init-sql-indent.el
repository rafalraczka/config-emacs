;;; init-sql-indent.el --- sql-indent configuration -*- lexical-binding: t -*-

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

(straight-use-package
 '(sql-indent :type git
              :host github
              :repo "alex-hhh/emacs-sql-indent"
              :files ("*" (:exclude ".git"))
              :fork (:repo "rafalraczka/emacs-sql-indent"
                     :branch "main")))

(with-eval-after-load 'sql-indent

  (setq-default sqlind-basic-offset 4)

  )

(provide 'init-sql-indent)

;;; init-sql-indent.el ends here
