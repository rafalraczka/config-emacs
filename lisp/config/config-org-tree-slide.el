;;; config-org-tree-slide.el --- org-tree-slide configuration -*- lexical-binding: t; -*-

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

(defvar-local config-org-tree-slide--original-var-values nil)

(defcustom config-org-tree-slide-minor-modes
  '((:disable blink-cursor-mode
              display-line-numbers-mode
              flyspell-mode)
    (:enable config-org-tree-slide-face-mode
             org-indent-mode
             text-scale-mode
             variable-pitch-mode
             visual-fill-column-mode
             visual-fill-mode
             visual-line-mode))
    "Modes which will be disabled or enabled with `org-tree-sldie-mode'.
Modes after disable keyword will be disabled on
`org-tree-slide-mode' enabling and disabled on its disabling.
The process will be reversed for modes after the enable keyword.")

(defcustom config-org-tree-slide-temp-faces
  `((org-drawer (:inherit org-hide))
    (org-level-1 (:height 1.2 :inherit org-level-1))
    (org-headline-done (:inherit unspecified))
    (org-table (:family "JuliaMono" :inherit nil))
    (org-tag (:inherit org-hide))
    (org-tree-slide-header-overlay-face
     (:height 1.4 :inherit org-document-title))
    )
  "")

(defcustom config-org-tree-slide-temp-variables
  '((line-spacing 5)
    (text-scale-mode-amount 2)
    (org-indent-indentation-per-level 0)
    (visual-fill-column-width 40)
    (visual-line-fringe-indicators nil)
    (visual-fill-column-center-text t)
    (org-hide-emphasis-markers t))
  "")

(define-minor-mode config-org-tree-slide-face-mode
  "Variable-pitch default-face mode.
An interface to `buffer-face-mode' which uses the `variable-pitch' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  :variable config-org-tree-slide-face-mode
  (let ((faces (when config-org-tree-slide-face-mode
                 config-org-tree-slide-temp-faces)))
    (setq-local face-remapping-alist faces)))

(defun config-org-tree-slide-toggle-minor-modes ()
  (dolist (list config-org-tree-slide-minor-modes)
    (let ((enable (eq (car list) :enable))
          (modes (cdr list)))
      (unless org-tree-slide-mode
        (setq enable (not enable)))
      (dolist (mode modes)
        (when (functionp mode)
          (if enable
              (funcall mode +1)
            (funcall mode -1)))))))

(defun config-org-tree-slide-set-variables ()
  (dolist (variable config-org-tree-slide-temp-variables)
    (let ((var (car variable))
          (val (car (cdr variable))))
      (if org-tree-slide-mode
          (progn
            (push (list var (symbol-value var))
                  config-org-tree-slide--original-var-values)
            (set-variable var val t))
        (let ((old-val (car (cdr (assoc var config-org-tree-slide--original-var-values)))))
          (set-variable var old-val t)))))
  (unless org-tree-slide-mode
    (setq config-org-tree-slide--original-var-values nil)))

(defun config-org-tree-slide-enable ()
  (org-display-inline-images))

(add-hook 'org-tree-slide-mode-hook #'config-org-tree-slide-toggle-minor-modes 20)
(add-hook 'org-tree-slide-mode-hook #'config-org-tree-slide-set-variables 10)
(add-hook 'org-tree-slide-play-hook #'config-org-tree-slide-enable)
(add-hook 'org-tree-slide-play-hook #'org-display-inline-images)
(add-hook 'org-tree-slide-stop-hook #'org-remove-inline-images)

(setq org-tree-slide-slide-in-effect nil)

;;; Footer:

(provide 'config-org-tree-slide)

;;; config-org-tree-slide.el ends here
