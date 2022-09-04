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

(defvar config-keymap-workspace-prefix [f5])
(defvar config-keymap-maps-map-prefix [f6])

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
(global-set-key config-keymap-maps-map-prefix 'config-keymap-maps-map)
(global-set-key config-keymap-workspace-prefix 'config-keymap-workspace-map)

;;;; Mode maps

;;;;; ace-window

(with-eval-after-load 'ace-window
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?f aw-flip-window)
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?e aw-execute-command-other-window "Execute Command Other Window")
          (?F aw-split-window-fair "Split Fair Window")
          (?2 aw-split-window-vert "Split Vert Window")
          (?3 aw-split-window-horz "Split Horz Window")
          (?1 delete-other-windows "Delete Other Windows")
          (?T aw-transpose-frame "Transpose Frame")
          ;; ?i ?r ?t are used by hyperbole.el
          (?? aw-show-dispatch-help)))
  (setq aw-keys '(?s ?e ?t ?n ?r ?i ?d ?h ?a ?o)))

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

;;;;; config-keymap-mail

(autoload 'notmuch-mua-new-mail "notmuch")

(let ((map (define-prefix-command 'config-keymap-mail-map)))
  (define-key map (kbd "h") 'notmuch-hello)
  (define-key map (kbd "j") 'notmuch-jump-search)
  (define-key map (kbd "m") 'notmuch-mua-new-mail)
  (define-key map (kbd "s") 'notmuch-search))

;;;;; config-keymap-maps-map

(let ((map (define-prefix-command 'config-keymap-maps-map)))
  (define-key map (kbd "g") 'config-keymap-git-map)
  (define-key map (kbd "h") 'config-keymap-help-map)
  (define-key map (kbd "m") 'config-keymap-mail-map)
  (define-key map (kbd "r") 'config-keymap-ref-map)
  )

;;;;; config-keymap-ref-map

(let ((map (define-prefix-command 'config-keymap-ref-map)))
  (define-key map (kbd "c") 'citar-copy-reference)
  (define-key map (kbd "i") 'citar-insert-citation)
  (define-key map (kbd "o") 'citar-open)
  (define-key map (kbd "n") 'citar-open-notes)
  (define-key map (kbd "l") 'citar-open-links))

;;;;; config-keymap-workspace-map

(defun config-keymap-workspace-switch-index (index)
  (exwm-workspace-switch-create index))

(let ((map (define-prefix-command 'config-keymap-workspace-map)))
  (define-key map (kbd "0") (lambda () (interactive) (config-keymap-workspace-switch-index 0)))
  (define-key map (kbd "1") (lambda () (interactive) (config-keymap-workspace-switch-index 1)))
  (define-key map (kbd "2") (lambda () (interactive) (config-keymap-workspace-switch-index 2)))
  (define-key map (kbd "3") (lambda () (interactive) (config-keymap-workspace-switch-index 3)))
  (define-key map (kbd "4") (lambda () (interactive) (config-keymap-workspace-switch-index 4)))
  (define-key map (kbd "5") (lambda () (interactive) (config-keymap-workspace-switch-index 5)))
  (define-key map (kbd "6") (lambda () (interactive) (config-keymap-workspace-switch-index 6)))
  (define-key map (kbd "7") (lambda () (interactive) (config-keymap-workspace-switch-index 7)))
  (define-key map (kbd "8") (lambda () (interactive) (config-keymap-workspace-switch-index 8)))
  (define-key map (kbd "9") (lambda () (interactive) (config-keymap-workspace-switch-index 9)))
  (define-key map (kbd "+") 'balance-windows)
  (define-key map (kbd "b") 'switch-to-buffer)
  (define-key map (kbd "k") 'kill-current-buffer)
  (define-key map (kbd "l") 'counsel-linux-app)
  (define-key map (kbd "n") 'tab-bar-switch-to-next-tab)
  (define-key map (kbd "p") 'tab-bar-switch-to-prev-tab)
  (define-key map (kbd "s") 'ace-window)
  (define-key map (kbd "s") 'window-toggle-side-windows)
  (define-key map (kbd "w") 'ace-window))

;; To use =s-p= while using =gnome= you have to disable system
;; shortcut for that keybinding first. It can be done with the
;; following command, seems to work on gnome 41.3.
;;
;; #+begin_src sh
;;   $ gsettings set org.gnome.mutter.keybindings switch-monitor '[]'
;; #+end_src

(global-set-key (kbd "s-+") 'balance-windows)
(global-set-key (kbd "s-0") (lambda () (interactive) (config-keymap-workspace-switch-index 0)))
(global-set-key (kbd "s-1") (lambda () (interactive) (config-keymap-workspace-switch-index 1)))
(global-set-key (kbd "s-2") (lambda () (interactive) (config-keymap-workspace-switch-index 2)))
(global-set-key (kbd "s-3") (lambda () (interactive) (config-keymap-workspace-switch-index 3)))
(global-set-key (kbd "s-4") (lambda () (interactive) (config-keymap-workspace-switch-index 4)))
(global-set-key (kbd "s-5") (lambda () (interactive) (config-keymap-workspace-switch-index 5)))
(global-set-key (kbd "s-6") (lambda () (interactive) (config-keymap-workspace-switch-index 6)))
(global-set-key (kbd "s-7") (lambda () (interactive) (config-keymap-workspace-switch-index 7)))
(global-set-key (kbd "s-8") (lambda () (interactive) (config-keymap-workspace-switch-index 8)))
(global-set-key (kbd "s-9") (lambda () (interactive) (config-keymap-workspace-switch-index 9)))
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-i") 'window-toggle-side-windows)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-l") 'counsel-linux-app)
(global-set-key (kbd "s-n") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-p") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-s") 'ace-window)
(global-set-key (kbd "s-w") 'ace-window)

;;; Footer:

(provide 'config-keymap)

;;; config-keymap.el ends here
