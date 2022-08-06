;;; config-exwm.el --- exwm configuration -*- lexical-binding: t; -*-

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

;;;; exwm

(defun config-exwm-mode-line-format ()
  (when (featurep 'doom-modeline)
    (setq-local column-number-mode nil)
    (setq-local doom-modeline-buffer-encoding nil)
    (setq-local doom-modeline-buffer-state-icon nil)
    (setq-local doom-modeline-major-mode-icon nil)
    (setq-local line-number-mode nil)))

(defun config-exwm-restart-server ()
  "Restart server."
  (server-force-delete)
  (server-start))

(defun config-exwm-run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (when (executable-find (nth 0 command-parts))
      (apply #'call-process `(,(car command-parts)
                              nil 0 nil
                              ,@(cdr command-parts))))))

(add-hook 'after-init-hook #'config-exwm-restart-server)
(add-hook 'exwm-mode-hook #'config-exwm-mode-line-format)

(setq exwm-input-global-keys
      (mapcar (lambda (i)
                `(,(kbd (format "s-%d" i)) .
                  (lambda ()
                    (interactive)
                    (exwm-workspace-switch-create ,i))))
              (number-sequence 0 9)))

(setq exwm-input-prefix-keys
      `(
        ,@(number-sequence ?\C-\s-a ?\C-\s-z)
        ,@(number-sequence ?\s-a ?\s-z)
        ?\C-c ?\C-h ?\C-u ?\C-x
        ?\M-! ?\M-& ?\M-: ?\M-` ?\M-x
        ?\s-+
        ))

(setq exwm-input-simulation-keys
      `(([?\C-a] . [home])
        ([?\C-b] . [left])
        ([?\C-e] . [end])
        ([?\C-f] . [right])
        ([?\C-n] . [down])
        ([?\C-p] . [up])
        ([?\C-s] . [?\C-f])
        ([?\C-w] . [?\C-x])
        ([?\C-y] . [?\C-v])
        ([?\M-_] . [?\C-y])
        ([?\M-w] . [?\C-c])
        ([?\C-_] . [?\C-z])))

(setq exwm-layout-show-all-buffers t)

;;;; exwm-randr

(require 'exwm-randr)

(defun config-exwm-randr-generate-plist (&optional monitors secondary-last max)
  "Generate plist of MONITORS and their workspaces.
MONITORS is optional and should be in a form of the
`config-exwm-randr-list-monitors' output. When SECONDARY-LAST is not
nil, move all the secondary monitors to the end of plist.  With
MAX user can define number of the last workspace, it is equal to
`exwm-workspace-number' when nil."
  (let* ((monitors (or monitors (config-exwm-randr-list-monitors)))
         (workspaces (or max exwm-workspace-number))
         (len (length monitors)))
    (when (> len 10)
      (user-error "Number of monitors unsupported (larger than 10)."))
    (if secondary-last
        (setq monitors (config-exwm-randr-reorder monitors workspaces)))
    (flatten-list (mapcar (lambda (a)
                            (list (nth 0 a) (nth 8 a)))
                          monitors))))

(defun config-exwm-randr-list-monitors ()
  (let* ((list (split-string
                (shell-command-to-string
                 "xrandr --listactivemonitors") "\n"))
         (seq (number-sequence 1 (- (length list) 2)))
         (list (mapcar (lambda (x) (nth x list)) seq)))
    (mapcar #'config-exwm-randr-split-monitor-string list)))

(defun config-exwm-randr-reorder (monitors workspaces)
  (let* ((len (length monitors))
         (main (assoc 0 monitors))
         (monitors (delete main monitors))
         (main-n (- workspaces (- len 1)))
         (main (make-list main-n main))
         (sequence (number-sequence 0 (1- workspaces))))
    (setq monitors `(,@main ,@monitors))
    (setq monitors
          (cl-loop for i from 0 to (1- workspaces)
                   collect (flatten-list `(,(nth i sequence)
                                           ,(cdr (nth i monitors))))))))

(defun config-exwm-randr-split-monitor-string (string)
  (let* ((string (cdr (split-string string "[ :+/\\tx]+")))
         (main (if (string-prefix-p "*" (nth 1 string)) "main" "secondary")))
    (setf (nth 0 string) (string-to-number (car string)))
    (setf (nth 1 string) main)
    string))

(defun config-exwm-randr-update-displays ()
  (config-exwm-run-in-background "autorandr --change --force")
  (setq exwm-randr-workspace-monitor-plist
        (config-exwm-randr-generate-plist nil t))
  (exwm-randr-refresh)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(add-hook 'exwm-randr-screen-change-hook #'config-exwm-randr-update-displays)

(setq exwm-randr-workspace-monitor-plist
      (config-exwm-randr-generate-plist nil t))

;;;; exwm-workspace

(defun config-exwm-workspace-rename-buffer ()
  (exwm-workspace-rename-buffer (concat exwm-title "-exwm")))

(add-hook 'exwm-update-title-hook #'config-exwm-workspace-rename-buffer)

(setq exwm-workspace-number 6)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-workspace-warp-cursor t)

;;;; init

(exwm-randr-enable)

(exwm-enable)

;;; Footer:

(provide 'config-exwm)

;;; config-exwm.el ends here
