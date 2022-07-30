;;; init-screenshot.el --- screenshot configuration -*- lexical-binding: t -*-

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

(straight-use-package
 '(screenshot :type git
              :host github
              :repo "tecosaur/screenshot"
              :bulid (:not compile)
              :fork (:repo "rafalraczka/screenshot")))

(with-eval-after-load 'screenshot

  (setq screenshot-border-width 0)
  (setq screenshot-shadow-offset-horizontal 0)
  (setq screenshot-shadow-offset-vertical 0)
  (setq screenshot-shadow-radius 0)

  (with-eval-after-load 'flyspell
    (defun my/screenshot-disable-flyspell-mode ()
      (flyspell-mode -1))
    (add-hook 'screenshot-buffer-creation-hook
              'my/screenshot-disable-flyspell-mode))

  (with-eval-after-load 'smartparens
    (defun my/screenshot-disable-show-smartparens-mode ()
      (show-smartparens-mode -1))
    (add-hook 'screenshot-buffer-creation-hook
              'my/screenshot-disable-show-smartparens-mode))

  )

(provide 'init-screenshot)

;;; init-screenshot.el ends here
