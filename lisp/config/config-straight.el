;;; config-straight.el --- straight profile -*- lexical-binding: t; -*-

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

(require 'core-straight)

(defvar config-straight-name 'config)

(defvar config-straight-version-lockfile-name
  (symbol-name config-straight-name))

(push '(source-hut "git.sr.ht" "") straight-hosts)

(defvar config-straight-recipes
  '(
    (git-email
     :host source-hut :repo "~yoctocell/git-email"
     :files ("git-email.el" "git-email-magit.el" "git-email-notmuch.el"))
    (lilypond
     :files ("elisp/*.el"))
    (modus-themes
     :type git :flavor melpa :host gitlab
     :repo "protesilaos/modus-themes" :branch "main"
     :fork (:repo "rafalraczka/modus-themes"))
    (olivetti
     :type git :flavor melpa :host github :repo "rnkn/olivetti"
     :fork (:repo "rafalraczka/olivetti"))
    (org-noter
     :type git :flavor melpa
     :host github :repo "weirdNox/org-noter"
     :fork (:repo "rafalraczka/org-noter"))
    (org-ref-prettify
     :type git :flavor melpa :host github :repo "alezost/org-ref-prettify.el"
     :fork (:host source-hut :repo "~rafalraczka/org-ref-prettify.el"
            :branch "main"))
    (org-roam
     :types git :flavor melpa :host github :repo "org-roam/org-roam"
     :files (:defaults "extensions/*" "org-roam-pkg.el")
     :fork (:repo "rafalraczka/org-roam"))
    (org-tree-slide
     :type git :flavor melpa :host github :repo "takaxp/org-tree-slide"
     :fork (:host source-hut :repo "~rafalraczka/org-tree-slide" :branch main))
    (screenshot
     :type git :host github :repo "tecosaur/screenshot" :bulid (:not compile)
     :fork (:repo "rafalraczka/screenshot"))
    (speed-type
     :type git :host github :repo "parkouss/speed-type"
     :fork (:repo "rafalraczka/speed-type"))
    (sql-indent
     :type git :host github :repo "alex-hhh/emacs-sql-indent"
     :files ("*" (:exclude ".git"))
     :fork (:repo "rafalraczka/emacs-sql-indent" :branch "main"))
    (yasnippet
     :fork (:repo "rafalraczka/yasnippet" :branch "main"))
    ))

(push (cons config-straight-name config-straight-version-lockfile-name)
      straight-profiles)

(push (cons config-straight-name config-straight-recipes)
      straight-recipe-overrides)

(setq straight-current-profile config-straight-name)

;;; Footer:

(provide 'config-straight)

;;; config-straight.el ends here
