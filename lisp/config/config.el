;;; config.el --- Modular configuration -*- lexical-binding: t; -*-

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

(require 'core)
(require 'config-straight)

(setq core-use-module-prefix 'config)

(core-use-module 'ace-window
  :after 'ace-window
  :install t)

(core-use-module 'all-the-icons
  :after 'all-the-icons
  :install t)

(core-use-module 'all-the-icons-dired
  :disable t
  :after 'dired
  :install t)

(core-use-module 'async
  :install t
  :execute ((add-hook 'dired-mode-hook #'dired-async-mode)))

(core-use-module 'beacon
  :disable t
  :after 'beacon
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'beacon-mode)))

(core-use-module 'bibtex-completion
  :disable t
  :after 'bibtex-completion
  :install t)

(core-use-module 'calendar
  :after 'calendar)

(core-use-module 'chemacs-tools
  :when core-envi-chemacs-directory
  :install 'local)

(core-use-module 'citar
  :after 'citar
  :install t)

(core-use-module 'company
  :after 'company
  :install t)

(core-use-module 'company-quickhelp
  :after 'company
  :install t)

(core-use-module 'counsel
  :install t)

(core-use-module 'ctrlf
  :after 'ctrlf
  :install t
  :execute ((add-hook 'after-init-hook #'ctrlf-mode)))

(core-use-module 'custom
  :after 'config)

(core-use-module 'desktop-environment
  :install t)

(core-use-module 'dired
  :after 'dired)

(core-use-module 'dired-collapse
  :after 'dired
  :install t)

(core-use-module 'dired-ranger
  :install t)

(core-use-module 'dired-single
  :install t)

(core-use-module 'doom-modeline
  :disable t
  :after 'config
  :install t)

(core-use-module 'ediff
  :after 'ediff)

(core-use-module 'editorconfig
  :install t)

(core-use-module 'eglot
  :install t)

(core-use-module 'elisp-mode
  :after 'elisp-mode)

(core-use-module 'embark
  :install t)

(core-use-module 'ement
  :install t)

(core-use-module 'ess
  :after 'ess
  :install t)

(core-use-module 'etc
  :after 'config)

(core-use-module 'exwm
  :when core-envi-exwm-required
  :install t
  :execute ((require 'config-exwm)))

(core-use-module 'face-remap
  :after 'face-remap)

(core-use-module 'faces
  :after 'config)

(core-use-module 'files
  :after 'config)

(core-use-module 'flycheck
  :install t)

(core-use-module 'forge
  :after 'magit
  :install t)

(core-use-module 'format-all
  :after 'format-all
  :install t)

(core-use-module 'gcmh
  :install t
  :execute ((add-hook 'after-init-hook #'gcmh-mode)))

(core-use-module 'git-email
  :install t)

(core-use-module 'git-timemachine
  :install t)

(core-use-module 'guix
  :install t)

(core-use-module 'help
  :after 'config)

(core-use-module 'helpful
  :install t)

(core-use-module 'hide-mode-line
  :install t)

(core-use-module 'hl-todo
  :after 'hl-todo
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'global-hl-todo-mode)))

(core-use-module 'ispell
  :after 'ispell)

(core-use-module 'julia-mode
  :install t)

(core-use-module 'keymap
  :after 'config)

(core-use-module 'langtool
  :after 'langtool
  :when (and core-envi-gnu-linux (executable-find "languagetool"))
  :install t)

(core-use-module 'lilypond
  :install t
  :execute ((autoload 'LilyPond-mode "lilypond-mode")
            (add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))))

(core-use-module 'lsp-julia
  :after 'julia-mode
  :install t)

(core-use-module 'lsp-mode
  :after 'lsp-mode
  :install t
  :execute ((setq lsp-keymap-prefix nil)))

(core-use-module 'lsp-pyls
  :after 'python)

(core-use-module 'lsp-sqls
  :after 'sqls)

(core-use-module 'magit
  :after 'magit
  :install t)

(core-use-module 'magit-todos
  :after 'magit
  :install t)

(core-use-module 'marginalia
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'marginalia-mode)))

(core-use-module 'minions
  :install t
  :execute ((add-hook 'after-init-hook #'minions-mode)))

(core-use-module 'modus-themes
  :after 'config
  :install t)

(core-use-module 'my
  :install 'local
  :execute ((add-hook 'exwm-init-hook #'my-polybar-start-panel)
            (setq ring-bell-function 'my-etc-blink-mode-line-bg)))

(core-use-module 'my-mini-gui
  :install 'local
  :execute ((if (daemonp)
                (add-hook 'server-after-make-frame-hook #'my-mini-gui-mode)
              (add-hook 'after-init-hook #'my-mini-gui-mode))))


(core-use-module 'notmuch
  :after 'notmuch
  :install t)

(core-use-module 'nov
  :install t
  :execute ((add-to-list 'auto-mode-alist
                         '("\\.epub\\'" . nov-mode))))

(core-use-module 'ol-notmuch
  :install t)

(core-use-module 'olivetti
  :after 'olivetti
  :install t
  :execute ((add-hook 'after-init-hook #'olivetti-global-mode)))

(core-use-module 'orderless
  :install t
  :execute ((setq completion-category-overrides
                  '((file (styles basic partial-completion))))
            (setq completion-styles '(orderless basic))))

(core-use-module 'org
  :after 'org
  :install t)

(core-use-module 'org-agenda
  :after 'org-agenda)

(core-use-module 'org-contrib
  :after 'org
  :install t)

(core-use-module 'org-noter
  :after 'org
  :install t)

(core-use-module 'org-pdftools
  :after 'org
  :install t)

(core-use-module 'org-ql
  :install t)

(core-use-module 'org-ref-prettify
  :install t)

(core-use-module 'org-roam
  :after 'org
  :install t
  :execute ((unless (file-exists-p (expand-file-name "org-roam.db"
                                                     user-emacs-directory))
              (add-hook 'after-init-hook #'org-roam-db-sync))))

(core-use-module 'org-roam-ui
  :install t)

(core-use-module 'org-super-agenda
  :after 'org-agenda
  :install t)

(core-use-module 'org-transclusion
  :install t)

(core-use-module 'org-tree-slide
  :after 'org
  :install t)

(core-use-module 'pdf-tools
  :after 'pdf-tools
  :install t
  :execute ((autoload 'pdf-view-mode "pdf-view")
            (if (file-exists-p (expand-file-name
                                "epdfinfo"
                                (straight--build-dir "pdf-tools")))
                (progn
                  (add-to-list 'auto-mode-alist
                               '("\\.[pP][dD][fF]\\'" . 'pdf-view-mode))
                  (add-to-list 'magic-mode-alist
                               '("%PDF" . pdf-view-mode)))
              (pdf-tools-install 'no-query-p))))

(core-use-module 'pico-dashboard
  :after 'config
  :install t)

(core-use-module 'presentation
  :after 'presentation
  :install t)

(core-use-module 'prog-mode
  :after 'prog-mode)

(core-use-module 'projectile
  :after 'projectile
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'projectile-mode)))

(core-use-module 'rainbow-delimiters
  :install t)

(core-use-module 'save-hist
  :execute ((add-hook 'core-utils-first-interaction-hook #'savehist-mode)))

(core-use-module 'screenshot
  :after 'screenshot
  :install t)

(core-use-module 'server
  :after 'config)

(core-use-module 'sh-script
  :after 'sh-script)

(core-use-module 'smartparens
  :install t)

(core-use-module 'speed-type
  :install t)

(core-use-module 'sql-indent
  :after 'sql-indent
  :install t)

(core-use-module 'startup
  :after 'config)

(core-use-module 'super-save
  :after 'config
  :install t)

(core-use-module 'tab-bar
  :after 'tab-bar)

(core-use-module 'text-mode
  :after 'text-mode)

(core-use-module 'tramp
  :execute ((setq tramp-default-method "ssh")))

(core-use-module 'undo-tree
  :after 'undo-tree
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'global-undo-tree-mode)))

(core-use-module 'vertico
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'vertico-mode)))

(core-use-module 'visual-fill
  :install t)

(core-use-module 'visual-fill-column
  :install t)

(core-use-module 'volatile-highlights
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'volatile-highlights-mode)))

(core-use-module 'vterm
  :install t)

(core-use-module 'which-key
  :after 'which-key
  :install t
  :execute ((add-hook 'core-utils-first-interaction-hook #'which-key-mode)))

(core-use-module 'ws-butler
  :install t
  :execute ((add-hook 'after-init-hook #'ws-butler-global-mode)))

(core-use-module 'yaml
  :install t)

(core-use-module 'yaml-mode
  :after 'yaml-mode
  :install t)

(core-use-module 'yasnippet
  :after 'config
  :install t
  :execute ((setq yas-alias-to-yas/prefix-p nil)))

;;; Footer:

(provide 'config)

;;; config.el ends here
