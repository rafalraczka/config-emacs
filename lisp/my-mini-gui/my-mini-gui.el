;;; my-mini-gui.el --- my-mini-gui configuration -*- lexical-binding: t; -*-

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

(defgroup my-mini-gui nil
  "Minimal Emacs GUI."
  :group 'frames)

(defcustom my-mini-gui-disabled-modes '(menu-bar-mode
                                        my-mini-gui-toggle-frame-decoration
                                        scroll-bar-mode
                                        tool-bar-mode
                                        tooltip-mode)
  "Modes which will be disabled with `my-mini-gui-mode'.
This variable can take list of any functions which accept
argument for default mode toggling, and only this argument, as a
first one, will be used.  -1 will be passed to these functions
while the `my-mini-gui-mode' is enabled, this will disable chosen
modes, and +1 will be passed when `my-mini-gui-mode' is disabled
and specified modes will be enabled."
  :type 'hook
  :group 'my-mini-gui)

(defvar my-mini-gui-global-mode nil)

(defun my-mini-gui-toggle-frame-decoration (&optional arg)
  "Toggle frame decorating elements of the Emacs GUI.
Enable decoration if ARG is nil, omitted, or is a positive
number.  Disable if ARG is a negative number."
  (if (and arg (<= arg 0))
      (push '(undecorated . t) default-frame-alist)
    (setq default-frame-alist (delete '(undecorated . t) default-frame-alist))))

;;;###autoload
(define-minor-mode my-mini-gui-mode
  "Mode which disable some of the Emacs GUI elements."
  :global t
  (if my-mini-gui-global-mode
      (mapc (lambda (f)
              (when (functionp f) (funcall f 1))) my-mini-gui-disabled-modes)
    (mapc (lambda (f)
            (when (functionp f) (funcall f -1))) my-mini-gui-disabled-modes))
  (setq my-mini-gui-global-mode (not my-mini-gui-global-mode)))

;;; Footer:

(provide 'my-mini-gui)

;;; my-mini-gui.el ends here
