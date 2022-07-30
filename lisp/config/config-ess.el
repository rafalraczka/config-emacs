;;; config-ess.el --- ess configuration -*- lexical-binding: t; -*-

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


(my/package-ensure 'ess)

(with-eval-after-load 'ess

  (defun my/ess-mode-config ()
    (ess-set-style 'C++ 'quiet))

  (add-hook 'ess-r-mode-hook 'my/ess-mode-config)

  (setq ess-eval-visibly nil)
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op% . nil)
          (ess-fl-keyword:fun-calls . nil)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . nil)
          (ess-fl-keyword:= . nil)
          (ess-R-fl-keyword:F&T . t)))

  ;; This is quick fix for Windows when Polish diacritics in data caused
  ;; problems.

  (when core-envi-windows
    (defun my/ess-r-set-iso-latin-1-coding-system ()
      (if core-envi-windows (set-buffer-file-coding-system 'iso-latin-1)))

    (add-hook 'ess-r-post-run-hook 'my/ess-r-set-iso-latin-1-coding-system))

  )

;;; Footer:

(provide 'config-ess)

;;; config-ess.el ends here
