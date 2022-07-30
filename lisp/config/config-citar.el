;;; config-citar.el --- citar configuration -*- lexical-binding: t -*-

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

(my/package-ensure 'citar)

(with-eval-after-load 'citar

  (setq citar-bibliography my-envi-bib-files)
  (setq citar-library-paths my-envi-bib-files-directories)
  (setq citar-notes-paths (list my-envi-ref-notes-directory))

  (with-eval-after-load 'config-embark
    (setq citar-at-point-function 'embark-act))

  (with-eval-after-load 'modus-themes
    (let ((file-col (cdr (assoc 'magenta-intense modus-themes-vivendi-colors)))
          (link-col (cdr (assoc 'blue-intense modus-themes-vivendi-colors)))
          (note-col (cdr (assoc 'cyan-intense modus-themes-vivendi-colors))))
      (setq citar-symbols
            `((file ,(propertize "F" 'face (list :foreground file-col)) . " ")
              (link ,(propertize "L" 'face (list :foreground link-col)) . " ")
              (note ,(propertize "N" 'face (list :foreground note-col)) . " ")))))

  )

(provide 'config-citar)

;;; config-citar.el ends here
