;;; config-help.el --- help configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka

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

(defun my/help-set-display-buffer-action ()
  (let* ((w (pcase (frame-width)
              ((pred (> 40)) 'display-buffer-same-window)
              ((pred (>= 160)) 0.5)
              ((pred (>= 240)) 80)
              ((pred (>= 320)) 0.33)
              ((pred (>= 400)) 120)))
         (action (if (symbolp w)
                     `("\\*Help.*"
                       (,w))
                   `("\\*Help.*"
                     (display-buffer-in-side-window)
                     (side . left)
                     (slot . -1)
                     (window-width . ,w)))))
    (push action
          display-buffer-alist)))

  (add-hook 'my-init-first-interaction-hook #'my/help-set-display-buffer-action)

  (provide 'config-help)

;;; config-help.el ends here
