;;; config-yaml-mode.el --- yaml configuration -*- lexical-binding: t; -*-

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

(defvar config-yaml-mode-outline-regexp
  "\\([^ ]+[:]\\)\\|\\(- [^ ]+\\)\\|\\([ ]*[-]*[ ]*[^ ]+\\)")

(defun config-yaml-mode-outline-level ()
  (let ((offset-string (make-string yaml-indent-offset 32)))
    (1+ (if (looking-at (format "%s+" offset-string))
            (/ (- (match-end 0) (match-beginning 0)) (length offset-string))
          0))))

(defun config-yaml-mode-config-outline ()
  (setq-local outline-regexp config-yaml-mode-outline-regexp)
  (setq-local outline-level #'config-yaml-mode-outline-level))

(add-hook 'yaml-mode-hook #'config-yaml-mode-config-outline)
(add-hook 'yaml-mode-hook #'outline-minor-mode)

;;; Footer:

(provide 'config-yaml-mode)

;;; config-yaml-mode.el ends here
