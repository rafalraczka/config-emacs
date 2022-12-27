;;; config-org-agenda.el --- org-agenda configuration -*- lexical-binding: t; -*-

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

(require 'all-the-icons)
(require 'org-super-agenda)
(require 'org-clock)

(defun my/org-agenda-mode-config ()
  (toggle-truncate-lines +1))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-mode-config)
(add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)

(setq org-agenda-block-separator "")
(setq org-agenda-breadcrumbs-separator " > ")
(setq org-agenda-current-time-string "<----- Now -----")
;; (setq org-agenda-deadline-leaders
;;      '("Deadline:  " "In %3d d.: "  "%2d d. ago: ")) ; Default
(setq org-agenda-follow-indirect t)
(setq org-agenda-log-mode-items '(clock))
(setq org-agenda-prefix-format " %-2i")
(setq org-agenda-remove-tags t)
(setq org-agenda-show-inherited-tags nil)
(setq org-agenda-span 7)                ; Show seven days by default
;; (setq org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: "))
(setq org-agenda-start-day "+0d")       ; Start on today
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-sticky t)
(setq org-agenda-timegrid-use-ampm nil) ; Set default time format to 24h
(setq org-agenda-time-grid '((today remove-match) ; Time scale
                             ()
                             "      "
                             "                "))
(setq org-agenda-time-leading-zero t) ; Show leading zeroes
(setq org-agenda-todo-keyword-format "%-4s")
(setq org-agenda-window-setup 'current-window)
(setq org-columns-default-format
      "%5CATEGORY(Category) %45ITEM(Task) %TODO %6Effort(Estim){:} %6CLOCKSUM(Clock) %TAGS")

(defvar my/org-agenda-header-starting-symbol "* ")

(defun my/org-agenda-header (string &optional new-line-end)
  "Make nice looking header with symbol at the beginning"
  (let ((header (concat my/org-agenda-header-starting-symbol string))
        (ending (when new-line-end "\n")))
    (concat header "\n" (make-string (1+ (string-width header)) ?\─) ending)))

(defvar my/org-agenda-header-calendar (my/org-agenda-header "Calendar" t))
(defvar my/org-agenda-header-projects (my/org-agenda-header "Projects" t))
(defvar my/org-agenda-header-todo (my/org-agenda-header "To Do" t))

(defvar my/org-agenda-default-agenda
  '((org-agenda-overriding-header my/org-agenda-header-calendar)
    (org-agenda-prefix-format "  %?-2t %s")
    ;; (org-agenda-prefix-format " %i %?-2t %s")
    (org-agenda-repeating-timestamp-show-all nil)
    (org-agenda-scheduled-leaders '("" ""))
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-skip-scheduled-if-done nil)
    (org-agenda-skip-timestamp-if-done t)
    (org-agenda-time)
    (org-super-agenda-groups nil)))

;; (cl-pushnew '("\\*Org Agenda.*\\*"
;;               (display-buffer-in-direction)
;;               (direction . left)
;;               (window-width . 0.33)
;;               (window-height . fit-window-to-buffer))
;;             display-buffer-alist)

(setq org-agenda-custom-commands
      `(("d" "Default agenda"
         ((agenda "" ,my/org-agenda-default-agenda)
          (tags-todo "-ROAM_TYPE=\"project\""
                     ((org-agenda-overriding-header my/org-agenda-header-todo)
                      (org-agenda-sorting-strategy '(priority-down category-keep))
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format " ")
                      ;; (org-agenda-prefix-format "%(my/org-agenda-get-progress)")
                      (org-agenda-todo-keyword-format "")
                      (org-super-agenda-groups my/org-super-agenda-groups)))
          (tags "ROAM_TYPE=\"project\""
                ((org-agenda-overriding-header my/org-agenda-header-projects)
                 (org-tags-match-list-sublevels nil)
                 ;; (org-agenda-prefix-format "%-2i %?b")
                 (org-agenda-prefix-format "")
                 (org-agenda-todo-keyword-format "%-4s")
                 (org-super-agenda-groups '((:auto-category t)))))))

        ("p" "Project agenda"
         ((agenda "" ,my/org-agenda-default-agenda)
          (tags-todo "ROAM_TYPE=\"task\""
                     ((org-agenda-overriding-header my/org-agenda-header-projects)
                      (org-super-agenda-groups '((:auto-parent t)))
                      (org-agenda-sorting-strategy '(todo-state-down priority-down))
                      (org-agenda-prefix-format "")))))

        ("e" "Low effort tasks"
         tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ("n" "Next Tasks"
         ((agenda "" ,my/org-agenda-default-agenda)
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))))

;;; Footer:

(provide 'config-org-agenda)

;;; config-org-agenda.el ends here
