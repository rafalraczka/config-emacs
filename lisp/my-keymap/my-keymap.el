;;; my-keymap.el --- Custom keybindings  -*- lexical-binding: t -*-

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

;;;; global-map -----------------------

(global-set-key (kbd "C-s-g") #'my-keymap-git-map)
(global-set-key (kbd "C-s-h") #'my-keymap-help-map)

;;;; my-keymap-git-map ----------------

(define-prefix-command 'my-keymap-git-map)

(with-eval-after-load 'init-git-timemachine
  (define-key my-keymap-git-map (kbd "t") #'git-timemachine))

(with-eval-after-load 'init-magit
  (define-key my-keymap-git-map (kbd "l f") #'magit-log-buffer-file)
  (define-key my-keymap-git-map (kbd "l c") #'magit-log-current)
  (define-key my-keymap-git-map (kbd "r") #'magit-rebase)
  (define-key my-keymap-git-map (kbd "s") #'magit-status))

;;;; my-keymap-help-map ---------------

(define-prefix-command 'my-keymap-help-map)

(set-keymap-parent my-keymap-help-map 'help-command)

(with-eval-after-load 'init-helpful
  (define-key my-keymap-help-map (kbd "f") #'helpful-callable)
  (define-key my-keymap-help-map (kbd "v") #'helpful-variable)
  (define-key my-keymap-help-map (kbd "k") #'helpful-key))

(provide 'my-keymap)

;;; my-keymap.el ends here
