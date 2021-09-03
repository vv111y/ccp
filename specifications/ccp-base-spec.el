;;; ccp-base-spec.el --- necessary minimal specification for a project  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Will Rempel

;; Author: Willy Rempel <willy.rempel@gmail.com>
;; URL: https://example.com/ccp-base-spec.el
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

(defvar ccp-base-spec-var nil
  "A variable.")

(defconst ccp-base-name (ccp-spec-create :name "Project Name"
                                         :key "Bn"
                                         :type 'string
                                         :description "Name for the project."))
(type-of ccp-base-name)
()
;;;; Functions

(defun cpp-new-base ()
  "Core execution function that takes the full specification and creates the new, empty project."
  (setq )(ccp-spec-create :name "Base Project"
                   :key "B"
                   :type 'ccp-spec
                   :action #'cpp-build-base
                   :parent 'self
                   :required '(ccp-base-name ccp-base-category ccp-base-labels ccp-base-language)
                   :optional '(ccp-base-dir ccp-base-description ccp-base-languages)))

(defun cpp-build-base (requireds &optional optionals) 
  ;; go through children, execute each required spec
  ;; iff success, execution action with required children returns as args
  )


(defun ccp-base-spec-foo (args)
  "Return foo for ARGS."
  (foo args))


(defun ccp-base-spec--bar (args)
  "Return bar for ARGS."
  (bar args))

;;;; Footer

(provide 'ccp-base-spec)

;;; ccp-base-spec.el ends here
