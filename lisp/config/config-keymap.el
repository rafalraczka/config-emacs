;;; config-keymap.el --- keymap configuration  -*- lexical-binding: t; -*-

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

;;;; global-map -----------------------

(global-set-key (kbd "C-s-g") #'config-keymap-git-map)
(global-set-key (kbd "C-s-h") #'config-keymap-help-map)

;;;; config-keymap-git-map ----------------

(define-prefix-command 'config-keymap-git-map)

(with-eval-after-load 'config-git-timemachine
  (define-key config-keymap-git-map (kbd "t") #'git-timemachine))

(with-eval-after-load 'config-magit
  (define-key config-keymap-git-map (kbd "l f") #'magit-log-buffer-file)
  (define-key config-keymap-git-map (kbd "l c") #'magit-log-current)
  (define-key config-keymap-git-map (kbd "r") #'magit-rebase)
  (define-key config-keymap-git-map (kbd "s") #'magit-status))

;;;; config-keymap-help-map ---------------

(define-prefix-command 'config-keymap-help-map)

(set-keymap-parent config-keymap-help-map 'help-command)

(with-eval-after-load 'config-helpful
  (define-key config-keymap-help-map (kbd "f") #'helpful-callable)
  (define-key config-keymap-help-map (kbd "v") #'helpful-variable)
  (define-key config-keymap-help-map (kbd "k") #'helpful-key))

;;; Footer:

(provide 'config-keymap)

;;; config-keymap.el ends here
