;;; config-face-remap.el --- face-remap configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>

;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://git.sr.ht/~rafalraczka/emacs-config

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

(with-eval-after-load 'face-remap

  (defcustom my/face-remap-variable-pitch-height-multiplier 1.05
    "The height of the `variable-pitch' as a multiplier of the `default' face height."
    :type 'number
    :group 'display)

  ;; TODO: Modify this approach to be local instead of global.
  (defface my/face-remap-fixed-pitch-copy '((t))
    "Copy of the `fixed-pitch' face.")
  (copy-face 'fixed-pitch 'my/face-remap-fixed-pitch-copy)

  (defun my/face-remap-set-default-fixed-pitch-height ()
    (custom-theme-set-faces
     'user
     '(fixed-pitch ((t :height 1.0)))))

  (defun my/face-remap-set-relative-pitch-height (&optional multiplier)
    (let* ((multi (or multiplier my/face-remap-variable-pitch-height-multiplier))
           (default-height (face-attribute 'default :height))
           (fixed-height-multiplier (/ 1 multi))
           (variable-family (face-attribute 'variable-pitch :family))
           (fixed-family (face-attribute 'default :family))
           (variable-height (round (* default-height multi))))
      (custom-theme-set-faces
       'user
       `(variable-pitch ((t (:family ,variable-family :height ,variable-height))))
       `(fixed-pitch ((t (:family ,fixed-family :height ,fixed-height-multiplier)))))))

  (defun my/face-remap-toggle-relative-height ()
    (if (and buffer-face-mode
             (string-equal buffer-face-mode-face "variable-pitch"))
        (my/face-remap-set-relative-pitch-height)
      (my/face-remap-set-default-fixed-pitch-height)))

  (add-hook 'buffer-face-mode-hook 'my/face-remap-toggle-relative-height)

  )

(provide 'config-face-remap)

;;; config-face-remap.el ends here
