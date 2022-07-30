;;; config-doom-modeline.el --- doom-modeline configuration -*- lexical-binding: t -*-

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

(my/package-ensure 'doom-modeline)

(defun my/doom-modeline-set-faces ()
  "Set faces for doom-modeline."
  (set-face-attribute 'doom-modeline-buffer-file nil :inherit 'unspecified)
  (set-face-attribute 'doom-modeline-buffer-major-mode nil :foreground 'unspecified :inherit 'unspecified)
  (set-face-attribute 'doom-modeline-buffer-modified nil :inherit 'unspecified)
  (set-face-attribute 'doom-modeline-buffer-path nil :inherit 'unspecified)
  (set-face-attribute 'doom-modeline-project-dir nil :inherit 'unspecified)
  (set-face-attribute 'doom-modeline-warning nil :inherit 'unspecified)
  (set-face-attribute 'mode-line nil :height 0.90)
  (set-face-attribute 'mode-line-inactive nil :height 1.0 :inherit 'mode-line))

(add-hook 'after-init-hook 'doom-modeline-mode)

(if my-envi-daemon
    (add-hook 'server-after-make-frame-hook 'my/doom-modeline-set-faces)
  (add-hook 'after-init-hook 'my/doom-modeline-set-faces 50))

(with-eval-after-load 'doom-modeline
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-github t)
  (setq doom-modeline-height 25)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-window-width-limit 80)
  (setq doom-modeline-workspace-name nil)

  (with-eval-after-load 'modus-themes
    (add-hook 'modus-themes-after-load-theme-hook 'my/doom-modeline-set-faces)))

(provide 'config-doom-modeline)

;;; config-doom-modeline.el ends here
