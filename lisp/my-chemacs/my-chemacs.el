;;; my-chemacs.el --- my-chemacs configuration -*- lexical-binding: t; -*-

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

(require 'chemacs)

;;;###autoload
(defun my-chemacs-read-profile (&optional prompt)
  "Read a PROFILE by its name.
PROMPT can overwrite the default prompt."
  (let ((profiles
         (with-temp-buffer
           (insert-file-contents chemacs-profiles-path)
           (goto-char (point-min))
           (read (current-buffer))))
        (prompt (or prompt "Profile: ")))
    (completing-read prompt profiles)))

;;;###autoload
(defun my-chemacs-run-emacs (profile)
  "Test Emacs startup with PROFILE."
  (interactive
   (list (my-chemacs-read-profile "Profile to run: ")))
  (let* ((cmd (format "emacs --fullscreen --with-profile %s" profile))
         cmd-parts)
    (when current-prefix-arg
      (setq cmd (format "%s %s" cmd "--debug-init")))
    (setq cmd-parts (split-string cmd "[ ]+"))
    (apply #'call-process `(,(car cmd-parts) nil 0 nil ,@(cdr cmd-parts)))))

;;; Footer:

(provide 'my-chemacs)

;;; my-chemacs.el ends here
