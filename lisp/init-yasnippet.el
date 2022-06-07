;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-

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
 '(yasnippet
   :fork (:repo "rafalraczka/yasnippet"
          :branch "main")))

(defun my/yasnippet-global-mode-no-message ()
  (let ((inhibit-message t)
	(message-log-max nil))
    (yas-global-mode)))

(add-hook 'my-init-first-interaction-hook #'my/yasnippet-global-mode-no-message)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
