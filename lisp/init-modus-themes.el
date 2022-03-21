;;; init-modus-themes.el --- modus-themes configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://github.com/rafalraczka/config-emacs

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
 '(modus-themes :type git
                :flavor melpa
                :branch "main"
                :host gitlab
                :repo "protesilaos/modus-themes"
                :fork (:repo "rafalraczka/modus-themes")))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'modus-themes-load-vivendi -50)
  (add-hook 'after-init-hook 'modus-themes-load-vivendi -50))

(provide 'init-modus-themes)

;;; init-modus-themes.el ends here
