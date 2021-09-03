;;; ccp-git-spec.el --- necessary minimal specification for a project  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Will Rempel

;; Author: Willy Rempel <willy.rempel@gmail.com>
;; URL: https://example.com/ccp-git-spec.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: tools  

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; This is a specification file for the ccp package. This specification defines the root, minimal, starting specification for a project.  

;;;; Installation

;;;; Usage

;; 

;;; Code:

;;;; Requirements

;;;; Variables

(defvar ccp-gitignore-repo nil "gitignore templates directory.")
(defvar ccp-gitignores-table nil "gitignore map filename to full path.")


;;;; Functions
;; initializes git and makes first commit of all said files.
;; (ccp-setup-git)
;; (ccp-init-commit)

;;;###autoload
(defun ccp-select-gitignore ()
  (interactive)
  (setq ccp-select-gitignores
        (completing-read-multiple
         "add gitignore templates: " (hash-table-keys ccp-gitignores-table))))

;;;###autoload
(defun ccp-update-gitignofre-templates ()
  "Update list of gitignore templates from the gitignore repo."
  (interactive)
  (when-let* ((gidir (concat ccp-root-dir "gitignore"))
              (f-dir? gidir)
              (gifiles (directory-files-recursively gidir ".gitignore" nil t)))
    (setq ccp-gitignores-table (make-hash-table :test 'equal))
    (mapc #'(lambda (el) (puthash (f-filename el) el ccp-gitignores-table))
          gifiles)))


;;;; Footer

(provide 'ccp-git-spec)

;;; ccp-git-spec.el ends here
