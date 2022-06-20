;;; init.el --- Initialization file for Emacs -*- lexical-binding: t -*-

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-init)
(require 'my-envi)
(require 'init-straight)
(require 'init-package)
(require 'init-server)

(when my-envi-exwm-required
   (require 'init-exwm))


(require 'init-ace-window)
(require 'init-all-the-icons)
(require 'init-all-the-icons-dired)
(require 'init-async)
(require 'init-calendar)
(require 'init-citar)
(require 'init-company)
(require 'init-company-quickhelp)
(require 'init-counsel)
(require 'init-ctrlf)
(require 'init-custom)
(require 'init-dired)
(require 'init-dired-ranger)
(require 'init-dired-single)
(require 'init-doom-modeline)
(require 'init-elfeed)
(require 'init-emacs-lisp)
(require 'init-embark)
(require 'init-ess)
(require 'init-faces)
(require 'init-files)
(require 'init-flycheck)
(require 'init-format-all)
(require 'init-git-timemachine)
(require 'init-help)
(require 'init-helpful)
(require 'init-hide-mode-line)
(require 'init-hl-todo)
(require 'init-indent)
(require 'init-ispell)
(require 'init-julia-mode)

(when (executable-find "languagetool")
  (require 'init-langtool))

(require 'init-lsp-mode)
(require 'init-lilypond)
(require 'init-lsp-julia)
(require 'init-lsp-pyls)
(require 'init-lsp-sqls)
(require 'init-magit)
(require 'init-magit-todos)
(require 'init-marginalia)
(require 'init-minions)
(require 'init-modus-themes)
(require 'init-notmuch)

(when (not my-envi-android)
  (require 'init-olivetti))

(require 'init-orderless)
(require 'init-org)
(require 'init-org-contrib)
(require 'init-org-noter)
(require 'init-org-roam)
(require 'init-org-roam-ui)
(require 'init-org-transclusion)
(require 'init-password-store)
(require 'init-pdf-tools)
(require 'init-presentation)
(require 'init-prog-mode)
(require 'init-projectile)
(require 'init-rainbow-delimiters)
(require 'init-savehist)
(require 'init-screenshot)
(require 'init-selectrum)
(require 'init-smartparens)
(require 'init-sql-indent)
(require 'init-startup)
(require 'init-tab-bar)
(require 'init-text-mode)
(require 'init-undo-tree)
;; (require 'init-vertico)
(require 'init-visual-fill)
(require 'init-which-key)
(require 'init-ws-butler)
(require 'init-yaml)
(require 'init-yaml-mode)
(require 'init-yasnippet)

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
