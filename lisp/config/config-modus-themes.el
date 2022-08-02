;;; config-modus-themes.el --- modus-themes configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://git.sr.ht/~rafalraczka/emacs-config

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

(defun my/modus-themes-set-faces ()
  "Set faces which overrides `modus-themes' defaults."
  (when (memq 'modus-vivendi custom-enabled-themes)
    (my/modus-themes-set-faces-vivendi))
  (set-face-attribute 'fringe nil :inherit 'line-number :background 'unspecified)
  (set-face-attribute 'mode-line nil :box 'unspecified)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-attribute 'modus-themes-tab-active nil :box 'unspecified)
  (set-face-attribute 'modus-themes-tab-backdrop nil
                      :background (face-attribute 'mode-line-inactive :background))
  (set-face-attribute 'modus-themes-tab-inactive nil :box 'unspecified)
  (set-face-attribute 'tab-bar-tab nil
                      :background (face-attribute 'mode-line :background)
                      :height 0.75)
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background (face-attribute 'mode-line-inactive :background)
                      :inherit 'unspecified
                      :height 0.75)
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-agenda-date-today nil :background 'unspecified)
    (set-face-attribute 'org-cite nil :inherit 'default)
    (set-face-attribute 'org-headline-done nil :inherit 'shadow)
    (set-face-attribute 'org-scheduled nil :foreground 'unspecified)
    (set-face-attribute 'org-tag nil :foreground nil :inherit 'shadow)))

(defun my/modus-themes-set-faces-vivendi ()
  "Set faces which overrides Modus Vivendi defaults."
  (set-face-attribute 'line-number nil :background "#1e1e1e")
  (set-face-attribute 'mode-line-inactive nil :background "#100f10")
  (set-face-attribute 'vertical-border nil :foreground "#1e1e1e")
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-block nil :background "#252525")
    (set-face-attribute 'org-block-begin-line nil :background "#323232")
    (set-face-attribute 'org-cite-key nil :foreground "#00d3d0"))
  (with-eval-after-load 'org-ref-ref-links
    (set-face-attribute 'org-ref-ref-face nil :foreground "#00d3d0")))

(add-hook 'modus-themes-after-load-theme-hook #'my/modus-themes-set-faces)

(setq modus-themes-headings (mapcar (lambda (i)
                                      `(,i . (rainbow)))
                                    (number-sequence 1 9)))
(setq modus-themes-hl-line '(intense))
(setq modus-themes-lang-checkers '(faint))
(setq modus-themes-links '(no-color))
(setq modus-themes-mode-line '(borderless))
(setq modus-themes-org-agenda '((scheduled . rainbow)
                                (header-date . (bold-today))))
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-paren-match '(intense))
(setq modus-themes-syntax '(yellow-comments))
(setq modus-themes-vivendi-color-overrides '((bg-main . "#191919")
                                             (bg-inactive . "#373737")
                                             (bg-tab-active . "#323232")
                                             (bg-tab-inactive . "#100f10")
                                             (fg-comment-yellow . "#d0ba95")))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'modus-themes-load-vivendi -50)
  (add-hook 'after-init-hook #'modus-themes-load-vivendi -50))

;;; Footer:

(provide 'config-modus-themes)

;;; config-modus-themes.el ends here
