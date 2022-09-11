;;; config-olivetti.el --- olivetti configuration -*- lexical-binding: t; -*-

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

(defun my/olivetti-set-body-width ()
  "Set olivetti body width according to `fill-column' value."
  (let* ((col (if (local-variable-p 'olivetti-body-width)
                  olivetti-body-width
                (or fill-column 80)))
         (width (if (and (boundp 'display-line-numbers-mode)
                         display-line-numbers-mode)
                    (+ col 4)
                  col)))
    (setq-local olivetti-body-width width)
    (setq-local olivetti-minimum-body-width width)))

(add-hook 'olivetti-mode-on-hook #'my/olivetti-set-body-width)

(setq olivetti-global-modes '(:exclude (exwm-mode
                                        image-mode
                                        pdf-view-mode)))

;;; Footer:

(provide 'config-olivetti)

;;; config-olivetti.el ends here
