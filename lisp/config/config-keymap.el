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

;;;;; ctl-x-map

(define-key ctl-x-map (kbd "C-z") nil)

;;;;; company-active-map

(with-eval-after-load 'company
  (let ((map company-active-map))
    (define-key map (kbd "M-n") nil)
    (define-key map (kbd "M-p") nil)))

;;;;; dired-mode-map

(with-eval-after-load 'dired
  (let ((map dired-mode-map))
    (define-key map (kbd "e") 'my/dired-create-dwim)
    (define-key map [remap dired-find-file] 'dired-single-buffer)
    (define-key map [remap dired-up-directory] 'dired-single-up-directory)
    (define-key map (kbd "C-w") 'dired-ranger-copy)
    (define-key map (kbd "C-y") 'dired-ranger-move)
    (define-key map (kbd "M-y") 'dired-ranger-paste)
    (setq dired-mode-map map)))

;;;;; emacs-lisp-mode-map

(with-eval-after-load 'elisp-mode
  (let ((map emacs-lisp-mode-map))
    (define-key map (kbd "C-c C-c") 'eval-buffer)
    (setq emacs-lisp-mode-map map)))

;;;;; exwm-mode-map

(with-eval-after-load 'exwm
  (let ((map exwm-mode-map))
    (define-key map [?\C-q] 'exwm-input-send-next-key)
    (setq exwm-mode map)))

;;;;; outline-minor-mode-map

(with-eval-after-load 'outline
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "<backtab>") 'outline-cycle-buffer)
    (define-key map (kbd "C-<return>") 'outline-insert-heading)
    (define-key map (kbd "C-<tab>") 'outline-cycle)
    (setf outline-mode-map map)))

;;;;; org-agenda-mode-amp

(with-eval-after-load 'org-agenda
  (let ((map org-agenda-mode-map))
    (define-key map (kbd "<ret>") 'org-agenda-tree-to-indirect-buffer)
    (setq org-agenda-mode-map map)))

;;;;; org-mode-map

(with-eval-after-load 'org
  (let ((map org-mode-map))
    (define-key map (kbd "C-c c") 'org-cite-insert)
    (define-key map (kbd "C-c i") 'org-roam-node-insert)
    (setq org-mode-map map)))

;;;;; vterm-mode-map

(with-eval-after-load 'vterm
  (let ((map vterm-mode-map))
    (define-key map (kbd "<f5>") nil)
    (define-key map (kbd "<f6>") nil)
    (define-key map (kbd "<f7>") nil)
    (define-key map (kbd "<f8>") nil)
    (define-key map (kbd "<f9>") nil)
    (setq vterm-mode-map map)))

;;;; Custom maps

;;;;; config-keymap-bookmark-map

(define-prefix-command 'config-keymap-bookmark-map)

(set-keymap-parent config-keymap-bookmark-map 'bookmark-map)

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
  (define-key map (kbd "p") 'config-keymap-project-map)
  (define-key map (kbd "r") 'config-keymap-ref-map)
  (define-key map (kbd "w") 'config-keymap-web-browser-map)
  )

;;;;; config-keymap-project-map

(let ((map (define-prefix-command 'config-keymap-project-map)))
  (define-key map (kbd "!") 'projectile-run-shell-command-in-root)
  (define-key map (kbd "&") 'projectile-run-async-shell-command-in-root)
  (define-key map (kbd "4 C-o") 'projectile-display-buffer)
  (define-key map (kbd "4 D") 'projectile-dired-other-window)
  (define-key map (kbd "4 a") 'projectile-find-other-file-other-window)
  (define-key map (kbd "4 b") 'projectile-switch-to-buffer-other-window)
  (define-key map (kbd "4 d") 'projectile-find-dir-other-window)
  (define-key map (kbd "4 f") 'projectile-find-file-other-window)
  (define-key map (kbd "4 g") 'projectile-find-file-dwim-other-window)
  (define-key map (kbd "4 t") 'projectile-find-implementation-or-test-other-window)
  (define-key map (kbd "5 D") 'projectile-dired-other-frame)
  (define-key map (kbd "5 a") 'projectile-find-other-file-other-frame)
  (define-key map (kbd "5 b") 'projectile-switch-to-buffer-other-frame)
  (define-key map (kbd "5 d") 'projectile-find-dir-other-frame)
  (define-key map (kbd "5 f") 'projectile-find-file-other-frame)
  (define-key map (kbd "5 g") 'projectile-find-file-dwim-other-frame)
  (define-key map (kbd "5 t") 'projectile-find-implementation-or-test-other-frame)
  (define-key map (kbd "<left>") 'projectile-previous-project-buffer)
  (define-key map (kbd "<right>") 'projectile-next-project-buffer)
  (define-key map (kbd "C") 'projectile-configure-project)
  (define-key map (kbd "D") 'projectile-dired)
  (define-key map (kbd "E") 'projectile-edit-dir-locals)
  (define-key map (kbd "ESC") 'projectile-project-buffers-other-buffer)
  (define-key map (kbd "F") 'projectile-find-file-in-known-projects)
  (define-key map (kbd "I") 'projectile-ibuffer)
  (define-key map (kbd "K") 'projectile-package-project)
  (define-key map (kbd "L") 'projectile-install-project)
  (define-key map (kbd "P") 'projectile-test-project)
  (define-key map (kbd "R") 'projectile-regenerate-tags)
  (define-key map (kbd "S") 'projectile-save-project-buffers)
  (define-key map (kbd "T") 'projectile-find-test-file)
  (define-key map (kbd "V") 'projectile-browse-dirty-projects)
  (define-key map (kbd "a") 'projectile-find-other-file)
  (define-key map (kbd "b") 'projectile-switch-to-buffer)
  (define-key map (kbd "c") 'projectile-compile-project)
  (define-key map (kbd "d") 'projectile-find-dir)
  (define-key map (kbd "e") 'projectile-recentf)
  (define-key map (kbd "f") 'projectile-find-file)
  (define-key map (kbd "g") 'project-find-regexp)
  (define-key map (kbd "i") 'projectile-invalidate-cache)
  (define-key map (kbd "j") 'projectile-find-tag)
  (define-key map (kbd "k") 'projectile-kill-buffers)
  (define-key map (kbd "l") 'projectile-find-file-in-directory)
  (define-key map (kbd "m") 'projectile-commander)
  (define-key map (kbd "o") 'projectile-multi-occur)
  (define-key map (kbd "p") 'projectile-switch-project)
  (define-key map (kbd "q") 'projectile-switch-open-project)
  (define-key map (kbd "r") 'projectile-replace)
  (define-key map (kbd "s g") 'projectile-grep)
  (define-key map (kbd "s r") 'projectile-ripgrep)
  (define-key map (kbd "s s") 'projectile-ag)
  (define-key map (kbd "t") 'projectile-toggle-between-implementation-and-test)
  (define-key map (kbd "u") 'projectile-run-project)
  (define-key map (kbd "v") 'projectile-vc)
  (define-key map (kbd "x e") 'projectile-run-eshell)
  (define-key map (kbd "x g") 'projectile-run-gdb)
  (define-key map (kbd "x i") 'projectile-run-ielm)
  (define-key map (kbd "x s") 'projectile-run-shell)
  (define-key map (kbd "x t") 'projectile-run-term)
  (define-key map (kbd "x v") 'projectile-run-vterm)
  (define-key map (kbd "z") 'projectile-cache-current-file))

;;;;; config-keymap-ref-map

(let ((map (define-prefix-command 'config-keymap-ref-map)))
  (define-key map (kbd "c") 'citar-copy-reference)
  (define-key map (kbd "i") 'citar-insert-citation)
  (define-key map (kbd "o") 'citar-open)
  (define-key map (kbd "n") 'citar-open-notes)
  (define-key map (kbd "l") 'citar-open-links))

;;;;; config-keymap-web-browser-map

(let ((map (define-prefix-command 'config-keymap-web-browser-map)))
  (define-key map (kbd "f") 'my-qutebrowser-switch-to-buffer)
  (define-key map (kbd "s") 'my-qutebrowser-start-process))

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
  (define-key map (kbd "r") 'config-keymap-bookmark-map)
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
(global-set-key (kbd "s-r") 'config-keymap-bookmark-map)
(global-set-key (kbd "s-s") 'ace-window)
(global-set-key (kbd "s-w") 'ace-window)

;;; Footer:

(provide 'config-keymap)

;;; config-keymap.el ends here
