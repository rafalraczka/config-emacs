;;; my-qutebrowser.el --- my-qutebrowser configuration -*- lexical-binding: t; -*-

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

(require 'exwm)
(require 'orderless)

;;;###autoload
(defun my-qutebrowser-switch-to-buffer (buffer-or-name)
  (interactive
   (let ((inhibit-quit t)
         (completion-styles '(orderless basic)))
     (minibuffer-with-setup-hook
         (lambda ()
           (insert "qutebrowser-exwm$ "))
       (progn
         (unless exwm-workspace-show-all-buffers
           (dolist (pair exwm--id-buffer-alist)
             (with-current-buffer (cdr pair)
               (when (= ?\s (aref (buffer-name) 0))
                 (let ((buffer-list-update-hook
                        (remq #'exwm-input--on-buffer-list-update
                              buffer-list-update-hook)))
                   (rename-buffer (substring (buffer-name) 1)))))))
         (prog1
             (with-local-quit
               (list (get-buffer (read-buffer-to-switch "Switch to buffer: "))))
           (unless exwm-workspace-show-all-buffers
             (dolist (pair exwm--id-buffer-alist)
               (with-current-buffer (cdr pair)
                 (unless (or (eq exwm--frame exwm-workspace--current)
                             (= ?\s (aref (buffer-name) 0)))
                   (let ((buffer-list-update-hook
                          (remq #'exwm-input--on-buffer-list-update
                                buffer-list-update-hook)))
                     (rename-buffer (concat " " (buffer-name)))))))))))))
  (exwm-workspace-switch-to-buffer buffer-or-name))

;;;###autoload
(defun my-qutebrowser-start-process ()
  (interactive)
  (start-process "" nil "qutebrowser"))

;;; Footer:

(provide 'my-qutebrowser)

;;; my-qutebrowser.el ends here
