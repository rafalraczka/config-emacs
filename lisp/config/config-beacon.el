;;; config-beacon.el --- Beacon configuration -*- lexical-binding: t; -*-

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

(my/package-ensure 'beacon)

(add-hook 'my-init-first-interaction-hook 'beacon-mode)

(with-eval-after-load 'beacon

  (defvar my/beacon-cursor-color
    (let* ((color (face-attribute 'cursor :background))
           (color-num (string-to-number (string-remove-prefix "#" color) 16)))
      (format "#%x" (1+ color-num))))

  (setq beacon-blink-delay 0.2)
  (setq beacon-blink-duration 0.5)
  (setq beacon-color my/beacon-cursor-color)
  (setq beacon-blink-when-point-moves-vertically 10)
  (setq beacon-push-mark 30)

  )

;;; Footer:

(provide 'config-beacon)

;;; config-beacon.el ends here
