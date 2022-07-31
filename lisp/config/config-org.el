;;; config-org.el --- org configuration -*- lexical-binding: t; -*-

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

(require 'citar)
(require 'oc-csl)

(defcustom my/org-fill-column 72
  "Default `fill-column' for Org mode."
  :type 'integer)

(defun my/org-mode-config ()
  (setq fill-column my/org-fill-column))

(add-hook 'org-clock-cancel-hook #'save-buffer)
(add-hook 'org-clock-in-hook #'save-buffer)
(add-hook 'org-clock-out-hook #'save-buffer)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'my/org-mode-config)
(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-cite-global-bibliography core-envi-bib-files)
(setq org-default-priority ?C)

;; Default directory for org files.

(setq org-directory core-envi-org-directory)
(setq org-ellipsis "…")

;; Hide all leading stars in a heading except the last one.

(setq org-hide-leading-stars t)
(setq org-highest-priority ?A)

;; Use id property when storing links to org entries and id property is
;; present.

(setq org-id-link-to-org-use-id 'use-existing)

(setq org-lowest-priority  ?E)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '((nil :maxlevel . 8)
        (org-agenda-files :maxlevel . 1)))
(setq org-refile-use-outline-path t)

;; - =CANC= - The task has been cancelled.
;;
;; - =DONE= - The task has been done.
;;
;; - =MYBE= - The task is optional and completion or participation is not
;;   required.
;;
;; - =STRT= - The task has been started but not finalized.
;;
;; - =TODO= - The task has to be done.
;;
;; - =WAIT= - Task cannot be done right now and I have to wait for some event
;;   to occur which would make this task actionable again e.g. somebody will
;;   do other task, some time will pass or the weather will change.

(setq org-todo-keywords
      '((sequence "TODO(t!)" "STRT(s!)" "|" "DONE(d!)")
        (sequence "MYBE(m!)" "WAIT(w!)" "|" "CANC(c!)" "UNDN(u!)")))

(push '(".+\\.org-.*"
        (display-buffer-in-direction)
        (direction . right)
        (window-width . 0.33))
      display-buffer-alist)

(org-babel-do-load-languages
 'org-babel-load-languages
 `(,@org-babel-load-languages
   (C . t)
   (R . t)
   (julia . t)
   (latex . t)
   (lilypond . t)
   (python . t)
   (shell . t)
   (sql . t)))

(when-let* ((path "/usr/share/java/ditaa/ditaa-0.11.jar")
            (_ (file-exists-p path)))
  (setq org-ditaa-jar-path path)
  (when-let* ((path-eps "/usr/share/java/ditaa-eps/DitaaEps.jar")
              (_ (file-exists-p path-eps)))
    (setq org-ditaa-eps-jar-path path-eps))
  (org-babel-do-load-languages 'org-babel-load-languages
                               `(,@org-babel-load-languages (ditaa . t))))

(setq org-cite-insert-processor 'citar)
(setq org-cite-follow-processor 'citar)
(setq org-cite-activate-processor 'citar)

;;; Footer:

(provide 'config-org)

;;; config-org.el ends here
