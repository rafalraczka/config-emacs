;;; init-prog-mode.el --- prog-mode configuration -*- lexical-binding: t -*-

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

(with-eval-after-load 'prog-mode

  (add-hook 'prog-mode-hook 'auto-fill-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (with-eval-after-load 'init-hl-todo
    (add-hook 'prog-mode-hook 'hl-todo-mode))

  (with-eval-after-load 'init-rainbow-delimiters
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

  (with-eval-after-load 'init-smartparens
    (add-hook 'prog-mode-hook 'smartparens-mode))

  )

(provide 'init-prog-mode)

;;; init-prog-mode.el ends here
