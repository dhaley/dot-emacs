;; Package --- Summary
;;; wg-projectile.el --- Workgroups integration with Projectile

;; Copyright (C) 2014 Damon Haley

;; Author: Damon Haley
;; Created: 2014-04-15
;; Keywords: project, convenience
;; Version: 0.1.0
;; Package-Requires: ((workgroups) (projectile "0.10.0") (cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library bridges workgroups mode to the bodacious library
;; Projectile.  The idea is to create a separate workgroups when
;; switching project.  A workgroup is an independant workspace for
;; Emacs, similar to multiple desktops in Gnome and MacOS.  I often
;; work on many projects at the same time, and using workgroups and
;; projectile together allows me to easily know which project I'm
;; current in, and focus on files that only belong to current project
;; when switching buffer.

;; To use this library, put this file in your Emacs load path, and
;; call (require 'wg-projectile)

;; See workgroups.el on github: https://github.com/nex3/workgroups-el

;;; Code:
(require 'workgroups)
(require 'projectile)

(workgroups-mode)

(defmacro projectile-wg-bridge (func-name)
  "Create advice to create a workgroup before invoking function FUNC-NAME.
The advice provides bridge between workgroups and projectile
functions when switch between projects.  After switching to a new
project, this advice creates a new workgroup for that project."
  `(defadvice ,func-name (before projectile-create-workgroups-after-switching-projects activate)
     "Create a dedicated workgroups for current project's window after switching projects."
     (let ((project-name (projectile-project-name)))
           (wg-create-workgroup project-name))))


(projectile-wg-bridge projectile-dired)
(projectile-wg-bridge projectile-find-file)


(defun projectile-wg-switch-project ()
  "Switch to a project or workgroups we have visited before. If the workgroups
of corresponding project does not exist, this function will call
`wg-switch-to-workgroup' to create one and swith to that before
`projectile-switch-project' invokes `projectile-switch-project-action'.
Otherwise, this function calls `wg-switch-to-workgroup' to an existing
workgroups of the project that we're switching to"
  (interactive)
  (let* ((project-to-switch
          (projectile-completing-read "Switch to project: "
                                      (projectile-relevant-known-projects)))
         (name (file-name-nondirectory (directory-file-name project-to-switch)))

         (wgroup (wg-get-workgroup 'name name t)))
    (message (concat name "is name"))
    (if wgroup
        (progn
         (message "me made it")
        (wg-switch-to-workgroup name))
      (message "no luck")
      (projectile-switch-project-by-name project-to-switch))))


(define-key projectile-mode-map (kbd "C-c p s") 'projectile-wg-switch-project)


(provide 'wg-projectile)
;;; wg-projectile.el ends here
