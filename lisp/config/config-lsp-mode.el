;;; config-lsp-mode.el --- lsp-mode configuration -*- lexical-binding: t; -*-

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

(require 'company)
(require 'which-key)

(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-signature-auto-activate nil)

(add-hook 'lsp-mode-hook #'company-mode)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;;; Footer:

(provide 'config-lsp-mode)

;;; config-lsp-mode.el ends here
