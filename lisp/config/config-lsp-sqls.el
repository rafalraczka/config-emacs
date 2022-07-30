;;; init-lsp-sqls.el --- lsp-sqls configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rafał Rączka

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

(require 'init-lsp-mode)

(setq lsp-sqls-workspace-config-path nil)

(with-eval-after-load 'sql

  (defun my/lsp-sqls-config-results-buffer ()
    "Configure buffer with results of the sql query."
    (when (string-equal (buffer-name) "*sqls results*")
      (setq truncate-lines t)
      (when (featurep 'olivetti)
        (olivetti-mode -1))))

  (add-hook 'help-mode-hook #'my/lsp-sqls-config-results-buffer 50)

  (add-hook 'sql-mode-hook #'lsp-deferred)

  )

(provide 'init-lsp-sqls)

;;; init-lsp-sqls.el ends here
