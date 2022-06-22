;;; init-dired.el --- dired configuration -*- lexical-binding: t -*-

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

(with-eval-after-load 'dired

  (defun my/dired-do-delete-permanently (&optional arg)
    (interactive "P")
    (let ((delete-by-moving-to-trash nil))
      (dired-do-delete arg)))

  (defun my/dired-enable-truncate-lines ()
    (setq truncate-lines t))

  (add-hook 'dired-after-readin-hook 'my/dired-enable-truncate-lines)

  ;; Use different approach to group directories first when =ls= is not
  ;; available.  This also set default switches.

  (if (executable-find "ls")
      (setq dired-listing-switches "-Ahl --group-directories-first")
    (progn (setq dired-listing-switches "-Ahl")
           (setq ls-lisp-dirs-first t)))

  (when my-envi-android
    (add-hook 'dired-mode-hook 'dired-hide-details-mode))

  )

(provide 'init-dired)

;;; init-dired.el ends here
