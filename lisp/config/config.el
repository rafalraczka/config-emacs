;;; config.el --- Modular configuration -*- lexical-binding: t; -*-

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

(require 'core)
(require 'config-straight)

(setq core-use-module-prefix 'config)

(core-use-module 'ace-window
  :after 'ace-window
  :install 'package)

(core-use-module 'all-the-icons
  :after 'all-the-icons
  :install 'package)

(core-use-module 'all-the-icons-dired
  :after 'dired
  :install 'package)

(core-use-module 'async
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'dired-async-mode)))

(core-use-module 'beacon
  :disable t
  :after 'beacon
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'beacon-mode)))

(core-use-module 'bibtex-completion
  :disable t
  :after 'bibtex-completion
  :install 'package)

(core-use-module 'calendar
  :after 'calendar)

(core-use-module 'citar
  :after 'citar
  :install 'straight)

(core-use-module 'company
  :after 'company
  :install 'package)

(core-use-module 'company-quickhelp
  :after 'company
  :install 'package)

(core-use-module 'counsel
  :install 'package)

(core-use-module 'ctrlf
  :after 'config
  :install 'package)

(core-use-module 'custom
  :after 'config)

(core-use-module 'dired
  :after 'dired)

(core-use-module 'dired-collapse
  :after 'dired
  :install 'package)

(core-use-module 'dired-ranger
  :install 'package)

(core-use-module 'dired-single
  :install 'package)

(core-use-module 'doom-modeline
  :disable t
  :after 'config
  :install 'package)

(core-use-module 'elisp
  :after 'elisp)

(core-use-module 'embark
  :install 'package)

(core-use-module 'ess
  :after 'ess
  :install 'package)

(core-use-module 'exwm
  :when core-envi-exwm-required
  :install 'straight
  :execute ((require 'config-exwm)))

(core-use-module 'face-remap
  :after 'face-remap)

(core-use-module 'faces
  :after 'config)

(core-use-module 'files
  :after 'config)

(core-use-module 'flycheck
  :install 'package)

(core-use-module 'forge
  :after 'magit
  :install 'package)

(core-use-module 'format-all
  :after 'format-all
  :install 'straight)

(core-use-module 'gcmh
  :install 'package
  :execute ((add-hook 'after-init-hook #'gcmh-mode)))

(core-use-module 'git-email
  :install 'straight)

(core-use-module 'git-timemachine
  :install 'straight)

(core-use-module 'help
  :after 'config)

(core-use-module 'helpful
  :install 'straight)

(core-use-module 'hide-mode-line
  :install 'package)

(core-use-module 'hl-todo
  :after 'hl-todo
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'global-hl-todo-mode)))

(core-use-module 'indent
  :after 'config)

(core-use-module 'ispell
  :after 'ispell)

(core-use-module 'julia-mode
  :install 'straight)

(core-use-module 'langtool
  :when (executable-find "languagetool")
  :install 'package)

(core-use-module 'lilypond
  :install 'straight
  :execute ((autoload 'LilyPond-mode "lilypond-mode")
            (add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))))

(core-use-module 'lsp-julia
  :after 'julia-mode
  :install 'package)

(core-use-module 'lsp-mode
  :after 'lsp-mode
  :install 'package)

(core-use-module 'lsp-pyls
  :after 'python)

(core-use-module 'lsp-sqls
  :after 'sqls)

(core-use-module 'magit
  :after 'magit
  :install 'straight)

(core-use-module 'magit-todos
  :after 'magit
  :install 'package)

(core-use-module 'marginalia
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'marginalia-mode)))

(core-use-module 'minions
  :install 'package
  :execute ((add-hook 'after-init-hook #'minions-mode)))

(core-use-module 'modus-themes
  :after 'config
  :install 'straight)

(core-use-module 'my-chemacs
  :when core-envi-chemacs-directory
  :install 'local)

(core-use-module 'my-etc
  :after 'config
  :install 'local)

(core-use-module 'my-keymap
  :after 'config
  :install 'local)

(core-use-module 'my-mini-gui
  :install 'local
  :execute ((if (daemonp)
                (add-hook 'server-after-make-frame-hook #'my-mini-gui-mode)
              (add-hook 'after-init-hook #'my-mini-gui-mode))))

(core-use-module 'my-polybar
  :when (and core-envi-exwm-required (executable-find "polybar"))
  :install 'local
  :execute ((add-hook 'exwm-init-hook #'my-polybar-start-panel)))

(core-use-module 'notmuch
  :after 'notmuch
  :install 'package)

(core-use-module 'ol-notmuch
  :install 'package)

(core-use-module 'olivetti
  :after 'config
  :install 'straight)

(core-use-module 'orderless
  :install 'package
  :execute ((setq completion-category-overrides
                  '((file (styles basic partial-completion))))
            (setq completion-styles '(orderless basic))))

(core-use-module 'org
  :after 'org
  :install 'straight)

(core-use-module 'org-contrib
  :after 'org
  :install 'package)

(core-use-module 'org-noter
  :after 'org
  :install 'straight)

(core-use-module 'org-ql
  :install 'straight)

(core-use-module 'org-roam
  :after 'org
  :install 'straight
  :execute ((unless (file-exists-p (expand-file-name "org-roam.db"
                                                     user-emacs-directory))
              (add-hook 'after-init-hook #'org-roam-db-sync))))

(core-use-module 'org-roam-ui
  :install 'package)

(core-use-module 'org-super-agenda
  :after 'org-agenda
  :install 'package)

(core-use-module 'org-transclusion
  :install 'straight)

(core-use-module 'pdf-tools
  :after 'pdf-tools
  :install 'package
  :execute ((add-hook 'after-init-hook #'pdf-loader-install)))

(core-use-module 'presentation
  :after 'presentation
  :install 'package)

(core-use-module 'prog-mode
  :after 'prog-mode)

(core-use-module 'projectile
  :after 'projectile
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'projectile-mode)))

(core-use-module 'rainbow-delimiters
  :install 'package)

(core-use-module 'save-hist
  :execute ((add-hook 'core-utils-first-interaction-hook #'savehist-mode)))

(core-use-module 'screenshot
  :after 'screenshot
  :install 'straight)

(core-use-module 'selectrum
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'selectrum-mode)))

(core-use-module 'server
  :after 'config)

(core-use-module 'sh-script
  :after 'sh-script)

(core-use-module 'smartparens
  :install 'package)

(core-use-module 'speed-type
  :install 'straight)

(core-use-module 'sql-indent
  :after 'sql-indent
  :install 'straight)

(core-use-module 'startup
  :after 'config)

(core-use-module 'super-save
  :after 'config
  :install 'package)

(core-use-module 'tab-bar
  :after 'tab-bar)

(core-use-module 'text-mode
  :after 'text-mode)

(core-use-module 'tramp
  :after 'config)

(core-use-module 'undo-tree
  :after 'undo-tree
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'global-undo-tree-mode)))

(core-use-module 'vertico
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'vertico-mode)))

(core-use-module 'visual-fill
  :install 'package)

(core-use-module 'volatile-highlights
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'volatile-highlights-mode)))

(core-use-module 'which-key
  :after 'which-key
  :install 'package
  :execute ((add-hook 'core-utils-first-interaction-hook #'which-key-mode)))

(core-use-module 'ws-butler
  :install 'package
  :execute ((add-hook 'after-init-hook #'ws-butler-global-mode)))

(core-use-module 'yaml
  :install 'package)

(core-use-module 'yaml-mode
  :install 'package)

(core-use-module 'yasnippet
  :after 'config
  :install 'straight)

;;; Footer:

(provide 'config)

;;; config.el ends here
