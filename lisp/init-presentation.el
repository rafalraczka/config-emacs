;;; init-presentation.el --- presentation configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka <info@rafalraczka.com>
;;
;; Author: Rafał Rączka <info@rafalraczka.com>
;; URL: https://github.com/rafalraczka/emacs-config

;; This file is NOT part of GNU Emacs.
;;
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

;; =presentation= package allows for global usage of the `text-scale-mode'
;; which effectively alter the height of the default face.  In other words,
;; this allows to enlarge text in all buffers at once while the
;; `text-scale-increase', `text-scale-decrease' and toggling `text-scale-mode'
;; works only locally.  This is an excellent solution to make a presentation
;; with Emacs when we would like to present more than a single buffer.

;;; Code:

(my/package-ensure 'presentation)

(with-eval-after-load 'presentation

  ;; `text-scale-mode-amount' equal to 2 is optimal most of the time for me.
  ;; For cases when it will not be enough it can be easily adjusted with
  ;; `presentation-mode' enabled.

  (setq presentation-default-text-scale 2)

  )

(provide 'init-presentation)

;;; init-presentation.el ends here
