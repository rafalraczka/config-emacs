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

;;;; global-map

(global-set-key (kbd "<XF86AudioLowerVolume>") 'desktop-environment-volume-decrement)
(global-set-key (kbd "<XF86AudioMicMute>") 'desktop-environment-toggle-microphone-mute)
(global-set-key (kbd "<XF86AudioMute>") 'desktop-environment-toggle-mute)
(global-set-key (kbd "<XF86AudioNext>") 'desktop-environment-music-next)
(global-set-key (kbd "<XF86AudioPlay>") 'desktop-environment-toggle-music)
(global-set-key (kbd "<XF86AudioPrev>") 'desktop-environment-music-previous)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'desktop-environment-volume-increment)
(global-set-key (kbd "<XF86AudioStop>") 'desktop-environment-music-stop)
(global-set-key (kbd "<XF86Bluetooth>") 'desktop-environment-toggle-bluetooth)
(global-set-key (kbd "<XF86MonBrightnessDown>") 'desktop-environment-brightness-decrement)
(global-set-key (kbd "<XF86MonBrightnessUp>") 'desktop-environment-brightness-increment)
(global-set-key (kbd "<XF86ScreenSaver>") 'desktop-environment-lock-screen)
(global-set-key (kbd "<XF86WLAN>") 'desktop-environment-toggle-wifi)
(global-set-key (kbd "<f9>") 'execute-extended-command)
(global-set-key (kbd "<print>") 'desktop-environment-screenshot)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-u") 'my-etc-upcase-previous-word)

;;;; Custom maps

;;;;; config-keymap-git-map

(let ((map (define-prefix-command 'config-keymap-git-map)))
  (define-key map (kbd "l") 'magit-log)
  (define-key map (kbd "r") 'magit-rebase)
  (define-key map (kbd "s") 'magit-status)
  (define-key map (kbd "t") 'git-timemachine))

;;;;; config-keymap-help-map

(define-prefix-command 'config-keymap-help-map)

(set-keymap-parent config-keymap-help-map 'help-command)

(define-key config-keymap-help-map (kbd "f") 'helpful-callable)
(define-key config-keymap-help-map (kbd "v") 'helpful-variable)
(define-key config-keymap-help-map (kbd "k") 'helpful-key)

;;; Footer:

(provide 'config-keymap)

;;; config-keymap.el ends here
