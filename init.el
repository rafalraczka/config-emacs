;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-init)
(require 'my-envi)
(require 'core-straight)
(require 'config-package)
(require 'config-server)

(when my-envi-exwm-required
   (require 'config-exwm))


(require 'config-ace-window)
(require 'config-all-the-icons)
(require 'config-all-the-icons-dired)
(require 'config-async)
(require 'config-calendar)
(require 'config-citar)
(require 'config-company)
(require 'config-company-quickhelp)
(require 'config-counsel)
(require 'config-ctrlf)
(require 'config-custom)
(require 'config-dired)
(require 'config-dired-collapse)
(require 'config-dired-ranger)
(require 'config-dired-single)
(require 'config-doom-modeline)
(require 'config-elfeed)
(require 'config-emacs-lisp)
(require 'config-embark)
(require 'config-ess)
(require 'config-face-remap)
(require 'config-faces)
(require 'config-files)
(require 'config-flycheck)
(require 'config-forge)
(require 'config-format-all)
(require 'config-git-timemachine)
(require 'config-help)
(require 'config-helpful)
(require 'config-hide-mode-line)
(require 'config-hl-todo)
(require 'config-indent)
(require 'config-ispell)
(require 'config-julia-mode)

(when (executable-find "languagetool")
  (require 'config-langtool))

(require 'config-lsp-mode)
(require 'config-lilypond)
(require 'config-lsp-julia)
(require 'config-lsp-pyls)
(require 'config-lsp-sqls)
(require 'config-magit)
(require 'config-magit-todos)
(require 'config-marginalia)
(require 'config-minions)
(require 'config-modus-themes)
(require 'config-notmuch)
(require 'config-ol-notmuch)

(when (not my-envi-android)
  (require 'config-olivetti))

(require 'config-orderless)
(require 'config-org)
(require 'config-org-contrib)
(require 'config-org-noter)
(require 'config-org-ql)
(require 'config-org-roam)
(require 'config-org-roam-ui)
(require 'config-org-super-agenda)
(require 'config-org-transclusion)
(require 'config-pdf-tools)
(require 'config-presentation)
(require 'config-prog-mode)
(require 'config-projectile)
(require 'config-rainbow-delimiters)
(require 'config-savehist)
(require 'config-screenshot)
(require 'config-selectrum)
(require 'config-smartparens)
(require 'config-speed-type)
(require 'config-sql-indent)
(require 'config-sh-script)
(require 'config-startup)
(require 'config-super-save)
(require 'config-tab-bar)
(require 'config-text-mode)
(require 'config-tramp)
(require 'config-undo-tree)
;; (require 'config-vertico)
(require 'config-visual-fill)
(require 'config-which-key)
(require 'config-ws-butler)
(require 'config-yaml)
(require 'config-yaml-mode)
(require 'config-yasnippet)

(when my-envi-chemacs-directory
  (require 'my-chemacs))

(require 'my-etc)
(require 'my-keymap)
(require 'my-mini-gui)

(when (and my-envi-exwm-required
           (executable-find "polybar"))
  (require 'my-polybar))

(provide 'init)

;;; init.el ends here
