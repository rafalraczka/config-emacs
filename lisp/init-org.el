;;; init-org.el --- org configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://github.com/rafalraczka/config-emacs

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

(straight-use-package 'org)

(with-eval-after-load 'org
  (defcustom my/org-fill-column 72
    "Default `fill-column' for Org mode."
    :type 'integer)

  (defun my/org-mode-config ()
    (setq fill-column my/org-fill-column))

  (add-hook 'org-mode-hook #'my/org-mode-config)

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
          (sequence "MYBE(m!)" "WAIT(w!)" "|" "CANC(c!)")))
  )

(provide 'init-org)

;;; init-org.el ends here
